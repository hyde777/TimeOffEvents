namespace TimeOff

open System

// First, we define our domain
type UserId = int

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

module HolidayTools =

    let isBetweenBoundaryOf = fun (holidayShorter:TimeOffHoliday) holidayLonger ->
        match holidayShorter, holidayLonger with
        | hs, hl when hs.Start > hl.Start && hs.End < hl.End -> true
        | _ -> false

    let takenByTheSameUser = fun (holiday1:TimeOffHoliday) holiday2 ->
        match holiday1, holiday2 with
        | hol1, hol2 when hol1.UserId = hol2.UserId -> true
        | _ -> false

    let isTheSameThanTheOther = fun holiday1 holiday2 ->
        match holiday1, holiday2 with
        | hol1, hol2 when hol1.Start = hol2.Start 
                          && hol1.End = hol2.End 
                          && takenByTheSameUser hol1 hol2 -> true
        | _ -> false
