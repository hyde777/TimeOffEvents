module TimeOff.Tests

open Expecto
open System

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
        UserId = 1
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = 1
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = 1
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }

    test "Requests on 2 overlaping days overlap" {
      let request1 = {
        UserId = 1
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 4); HalfDay = PM }
      }

      let request2 = {
        UserId = 1
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
    test "A request is created" {
      let request = {
        UserId = 1
        HolidayId =  Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee 1)
      |> AndDateIs (2018, 12, 3)
      |> When (AskHolidayTimeOff request)
      |> Then (Ok [HolidayCreated request]) "The request should have been created"
    }

    test "A request in the past cannot be created" {
      let request = {
        UserId = 1
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 11, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 11, 28); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee 1)
      |> AndDateIs (2018, 12, 3)
      |> When (AskHolidayTimeOff request)
      |> Then (Error "The holiday starts in the past") "The request should not have been created"
    }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request created is validated" {
      let request = {
        UserId = 1
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ HolidayCreated request ] // L'event qui rentre
      |> ConnectedAs Manager
      |> AndDateIs (2018, 12, 3)
      |> When (ValidateHoliday (1, request.HolidayId)) // quand cette commande est rentr�
      |> Then (Ok [HolidayValidated request]) "The request should have been validated"
    }
  ]

[<Tests>]
let askCancelationTests =
  testList "Ask Cancelation tests" [
    test "A User ask a cancelation of his holiday" {
      let request = {
        UserId = 1
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }
        
      Given [ HolidayValidated request ]
      |> ConnectedAs (Employee 1)
      |> AndDateIs (2018, 12, 29)
      |> When (AskCancelHoliday (request.UserId, request.HolidayId))
      |> Then (Ok [HolidayCancelPending request]) "The request should have been canceled"
    }
  ]

[<Tests>]
let denyCancelationTests =
  testList "Deny Cancelation tests" [
    test "A Manager deny a cancelation of an Employee holiday" {
      let request = {
        UserId = 1
        HolidayId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }
        
      Given [ HolidayCancelPending request ]
      |> ConnectedAs (Manager)
      |> AndDateIs (2018, 12, 29)
      |> When (DenyCancelHoliday (request.UserId, request.HolidayId))
      |> Then (Ok [HolidayDenyCancel request]) "The ask of holiday cancelation should have been canceled/denied "
    }
  ]
