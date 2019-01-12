module TimeOff.Tests

open Expecto
open System
open HolidayTools

let Given (events: HolidayEvent list) = events
let ConnectedAs (user: User) (events: HolidayEvent list) = events, user
let AndDateIs (year, month, day) (events: HolidayEvent list, user: User) = events, user, DateTime(year, month, day)
let When (command: Command) (events: HolidayEvent list, user: User, today: DateTime) = events, user, today, command
let Then expected message (events: HolidayEvent list, user: User, today: DateTime, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserHolidaysState>) (event: HolidayEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide today userRequestsState user command
    Expect.equal result expected message

open System

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }

    test "Requests on 2 overlaping days overlap" {
      let request1 = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 4); HalfDay = PM }
      }

      let request2 = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlap"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "An holiday is created" {
      let holiday = {
        UserId = "joed"
        HolidayId =  Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee (holiday.UserId, DateTime(2018, 12, 2)))
      |> AndDateIs (2018, 12, 3)
      |> When (AskHolidayTimeOff holiday)
      |> Then (Ok [HolidayCreated holiday]) "The request should have been created"
    }

    test "A request in the past cannot be created" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 11, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 11, 28); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee (holiday.UserId, DateTime(2018, 12, 2)))
      |> AndDateIs (2018, 12, 3)
      |> When (AskHolidayTimeOff holiday)
      |> Then (Error "The holiday starts in the past") "The request should not have been created"
    }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A holiday created is validated" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ HolidayCreated holiday ] // L'event qui rentre
      |> ConnectedAs Manager
      |> AndDateIs (2018, 12, 3)
      |> When (ValidateHoliday ("joed", holiday.HolidayId)) // quand cette commande est rentr�
      |> Then (Ok [HolidayValidated holiday]) "The request should have been validated"
    }
  ]

[<Tests>]
let refusedTests =
  testList "Refused tests" [
    test "A holiday created is refused" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ HolidayCreated holiday ]
      |> ConnectedAs Manager
      |> AndDateIs (2018, 12, 3)
      |> When (RefuseHoliday ("joed", holiday.HolidayId))
      |> Then (Ok [HolidayRefused holiday]) "The request should have been validated"
    }
  ]

[<Tests>]
let askCancelationTests =
  testList "Ask Cancelation tests" [
    test "A User ask a cancelation of his validated holiday" {
      let request = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }
        
      Given [ HolidayValidated request ]
      |> ConnectedAs (Employee ("joed", DateTime(2018, 12, 2)))
      |> AndDateIs (2018, 12, 27)
      |> When (AskCancelHoliday (request.UserId, request.HolidayId))
      |> Then (Ok [HolidayCancelPending request]) "The request should have been canceled"
    }

    test "A User ask a cancelation of his ask of holiday" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }
        
      Given [ HolidayCreated holiday ]
      |> ConnectedAs (Employee (holiday.UserId, DateTime(2018, 12, 2)))
      |> AndDateIs (2018, 12, 27)
      |> When (AskCancelHoliday (holiday.UserId, holiday.HolidayId))
      |> Then (Ok [HolidayCancelPending holiday]) "The request should have been canceled"
    }
  ]

// TODO: Ask teacher about the after of the deny
[<Tests>]
let denyCancelationTests =
  testList "Deny Cancelation tests" [
    test "A Manager deny a cancelation of an Employee holiday" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }
        
      Given [ HolidayCancelPending holiday ]
      |> ConnectedAs (Manager)
      |> AndDateIs (2018, 12, 29)
      |> When (DenyCancelHoliday (holiday.UserId, holiday.HolidayId))
      |> Then (Ok [HolidayDenyCancel holiday]) "The ask of holiday cancelation should have been canceled/denied"
    }
  ]

[<Tests>]
let ManagerCancelationTests =
  testList "Manager Cancelation tests" [
    test "A Manager confirm an ask of cancelation from an Employee" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }
        
      Given [ HolidayCancelPending holiday ]
      |> ConnectedAs (Manager)
      |> AndDateIs (2018, 12, 29)
      |> When (CancelHoliday (holiday.UserId, holiday.HolidayId))
      |> Then (Ok [HolidayCancel holiday]) "The ask of holiday cancelation is confirmed"
    }

    test "A Manager cancel a validated Holiday" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }
        
      Given [ HolidayValidated holiday ]
      |> ConnectedAs (Manager)
      |> AndDateIs (2018, 12, 29)
      |> When (CancelHoliday (holiday.UserId, holiday.HolidayId))
      |> Then (Ok [HolidayCancel holiday]) "The ask of holiday cancelation is confirmed"
    }

    test "A Manager cancel an Holiday waiting to be validated" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }
        
      Given [ HolidayCreated holiday ]
      |> ConnectedAs (Manager)
      |> AndDateIs (2018, 12, 29)
      |> When (CancelHoliday (holiday.UserId, holiday.HolidayId))
      |> Then (Ok [HolidayCancel holiday]) "The ask of holiday cancelation is confirmed"
    }
  ]

[<Tests>]
let EmployeeCancelationTests =
  testList "Employee Cancelation tests" [
    test "An Employee cancel his validated Holiday" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }
        
      Given [ HolidayValidated holiday ]
      |> ConnectedAs (Employee (holiday.UserId, DateTime(2018, 12, 2)))
      |> AndDateIs (2018, 12, 27)
      |> When (CancelHoliday (holiday.UserId, holiday.HolidayId))
      |> Then (Ok [HolidayCancel holiday]) "The ask of holiday cancelation is confirmed"
    }

    test "An Employee cancel an Holiday waiting to be validated" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }
        
      Given [ HolidayCreated holiday ]
      |> ConnectedAs (Employee (holiday.UserId, DateTime(2018, 12, 2)))
      |> AndDateIs (2018, 12, 27)
      |> When (CancelHoliday (holiday.UserId, holiday.HolidayId))
      |> Then (Ok [HolidayCancel holiday]) "The ask of holiday cancelation is confirmed"
    }
  ]

[<Tests>]
let AskBalance =
  testList "Ask an employee balance tests" [
    test "An Employee ask his balance Holiday when he only have 1 holliday" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 15); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 17); HalfDay = PM } }
        
      let mutable spanOfTime : float = float (holiday.End.Date - holiday.Start.Date).Days
      if holiday.End.HalfDay = AM && holiday.Start.HalfDay = AM 
      || holiday.End.HalfDay = PM && holiday.Start.HalfDay = PM then
        spanOfTime <- (spanOfTime + 0.5)

      let userVacationBalance : UserVacationBalance = {
        UserName = holiday.UserId
        BalanceYear = 2018
        CarriedOver = 0.0
        PortionAccruedToDate = 24.0
        TakenToDate = spanOfTime
        Planned = 0.0
        CurrentBalance = 24.0 - spanOfTime
      }

      Given [ HolidayValidated holiday ]
      |> ConnectedAs (Employee (holiday.UserId, DateTime(2018, 12, 2)))
      |> AndDateIs (2018, 12, 29)
      |> When (GetBalance holiday.UserId)
      |> Then (Ok [ HolidayBalance userVacationBalance ]) "We got a balance is confirmed"
    }

    test "Employee ask his balance Holiday when he have multiple holliday" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 15); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 17); HalfDay = PM } }
       
      let holiday2 = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 11, 15); HalfDay = AM }
        End = { Date = DateTime(2018, 11, 17); HalfDay = PM }
      }

      let holiday3 = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 15); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 17); HalfDay = PM }
      }
      
      let mutable spanOfTime = getSpanOfHoliday holiday
      spanOfTime <- spanOfTime + getSpanOfHoliday holiday2
      spanOfTime <- spanOfTime + getSpanOfHoliday holiday3

      let userVacationBalance : UserVacationBalance = {
        UserName = holiday.UserId
        BalanceYear = 2018
        CarriedOver = 0.0
        PortionAccruedToDate = 24.0
        TakenToDate = spanOfTime
        Planned = 0.0
        CurrentBalance = 24.0 - spanOfTime
      }

      Given [ HolidayValidated holiday; HolidayValidated holiday2; HolidayValidated holiday3 ]
      |> ConnectedAs (Employee (holiday.UserId, DateTime(2018, 12, 2)))
      |> AndDateIs (2018, 12, 29)
      |> When (GetBalance holiday.UserId)
      |> Then (Ok [ HolidayBalance userVacationBalance ]) "We got a balance is confirmed"
    }

    test "Employee ask his balance Holiday when he have multiple holliday and taken holliday" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 1, 15); HalfDay = AM }
        End = { Date = DateTime(2019, 1, 17); HalfDay = PM } }
       
      let holiday2 = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 11, 15); HalfDay = AM }
        End = { Date = DateTime(2018, 11, 17); HalfDay = PM }
      }

      let holiday3 = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 15); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 17); HalfDay = PM }
      }
      
      let mutable spanOfTime = getSpanOfHoliday holiday
      let mutable spanOfTimeBefore = getSpanOfHoliday holiday2
      spanOfTimeBefore <- spanOfTimeBefore + getSpanOfHoliday holiday3


      let userVacationBalance : UserVacationBalance = {
        UserName = holiday.UserId
        BalanceYear = 2019
        CarriedOver = 24.0 - spanOfTimeBefore
        PortionAccruedToDate = 24.0
        TakenToDate = spanOfTime
        Planned = 0.0
        CurrentBalance = (24.0 + (24.0 - spanOfTimeBefore)) - (0.0 + spanOfTime)
      }

      Given [ HolidayCreated holiday; HolidayValidated holiday2; HolidayValidated holiday3 ]
      |> ConnectedAs (Employee (holiday.UserId, DateTime(2017, 12, 1)))
      |> AndDateIs (2019, 12, 29)
      |> When (GetBalance holiday.UserId)
      |> Then (Ok [ HolidayBalance userVacationBalance ]) "We got a balance is confirmed"
    }

    test "Employee ask his balance Holiday when he have multiple holliday and taken holliday and plannedHoliday" {
      let holiday = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 1, 15); HalfDay = AM }
        End = { Date = DateTime(2019, 1, 17); HalfDay = PM } }
       
      let holiday2 = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 11, 15); HalfDay = AM }
        End = { Date = DateTime(2018, 11, 17); HalfDay = PM }
      }

      let holiday3 = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 15); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 17); HalfDay = PM }
      }
      
      let holidayPlanned = {
        UserId = "joed"
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 15); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 17); HalfDay = PM }
      }

      let mutable spanOfTime = getSpanOfHoliday holiday
      let mutable spanOfTimeBefore = getSpanOfHoliday holiday2
      spanOfTimeBefore <- spanOfTimeBefore + getSpanOfHoliday holiday3
      let mutable spanOfTimePlanned = getSpanOfHoliday holidayPlanned

      let userVacationBalance : UserVacationBalance = {
        UserName = holiday.UserId
        BalanceYear = 2019
        CarriedOver = 24.0 - spanOfTimeBefore
        PortionAccruedToDate = 24.0
        TakenToDate = spanOfTime
        Planned = spanOfTimePlanned
        CurrentBalance = (24.0 + (24.0 - spanOfTimeBefore)) - (spanOfTimePlanned + spanOfTime)
      }

      Given [ HolidayCreated holiday; HolidayValidated holiday2; HolidayValidated holiday3; HolidayValidated holidayPlanned ]
      |> ConnectedAs (Employee (holiday.UserId, DateTime(2017, 12, 1)))
      |> AndDateIs (2019, 4, 29)
      |> When (GetBalance holiday.UserId)
      |> Then (Ok [ HolidayBalance userVacationBalance ]) "We got a balance is confirmed"
    }
  ]