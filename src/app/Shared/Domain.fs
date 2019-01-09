namespace TimeOff

open System

// First, we define our domain
type UserId = string

type User =
    | Employee of UserId
    | Manager

type HalfDay = | AM | PM

[<CLIMutable>]
type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

[<CLIMutable>]
type TimeOffHoliday = {
    UserId: UserId
    HolidayId: Guid
    Start: Boundary
    End: Boundary
}

[<CLIMutable>]
type UserVacationBalance = {
  UserName : UserId
  BalanceYear: int
  CarriedOver: float
  PortionAccruedToDate: float
  TakenToDate: float
  CurrentBalance: float
}

module HolidayTools =
    
    let supBound bound1 bound2 =
        match bound1, bound2 with
        | b1, b2 when b1.Date > b2.Date -> true
        | b1, b2 when b1.Date < b2.Date -> false
        | b1, b2 when b1.HalfDay = PM && b2.HalfDay = AM -> true
        | _ -> false

    let infBound bound1 bound2 =
        match bound1, bound2 with
        | b1, b2 when b1.Date > b2.Date -> false
        | b1, b2 when b1.Date < b2.Date -> true
        | b1, b2 when b1.HalfDay = PM && b2.HalfDay = AM -> false
        | _ -> true
        
    let isBetweenBoundaryOf = fun (holiday1:TimeOffHoliday) holiday2 ->
        match holiday1, holiday2 with
        | hol1, hol2 when supBound hol1.Start hol2.Start && infBound hol1.Start hol2.End -> true
        | hol1, hol2 when supBound hol2.Start hol1.Start && infBound hol2.Start hol1.End -> true
        | hol1, hol2 when supBound hol1.End hol2.Start && infBound hol1.End hol2.End -> true
        | hol1, hol2 when supBound hol2.End hol1.Start && infBound hol2.End hol1.End -> true
        | _ -> false
    
    let takenByTheSameUser = fun (holiday1:TimeOffHoliday) holiday2 ->
        match holiday1, holiday2 with
        | hol1, hol2 when hol1.UserId = hol2.UserId -> true
        | _ -> false        
