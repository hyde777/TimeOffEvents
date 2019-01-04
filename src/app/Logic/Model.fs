namespace TimeOff

open System
open EventStorage
open HolidayTools
// Then our commands (ce qu'on fait)
type Command =
    | AskHolidayTimeOff of TimeOffHoliday
    | ValidateHoliday of UserId * Guid 
    | AskCancelHoliday of UserId * Guid
    | DenyCancelHoliday of UserId * Guid
    with member this.UserId =
            match this with
            | AskHolidayTimeOff holiday -> holiday.UserId
            | ValidateHoliday (userId, _) -> userId
            | AskCancelHoliday (userId, _) -> userId
            | DenyCancelHoliday (userId, _) -> userId


// And our events (ce qu'on écoute)
type HolidayEvent =
    | HolidayCreated of TimeOffHoliday
    | HolidayValidated of TimeOffHoliday 
    | HolidayCancelPending of TimeOffHoliday
    | HolidayDenyCancel of TimeOffHoliday
    with member this.Request =
            match this with
            | HolidayCreated holiday -> holiday
            | HolidayValidated holiday -> holiday
            | HolidayCancelPending holiday -> holiday
            | HolidayDenyCancel holiday -> holiday

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =
    type HolidayState =
        | NotCreated
        | PendingValidation of TimeOffHoliday
        | Validated of TimeOffHoliday
        | PendingCancelation of TimeOffHoliday
        | DeniedCancelation of TimeOffHoliday with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation holiday
            | Validated holiday 
            | PendingCancelation holiday
            | DeniedCancelation holiday -> holiday
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | Validated _ 
            | PendingCancelation _ 
            | DeniedCancelation _ -> true

    type UserHolidaysState = Map<Guid, HolidayState>

    let evolveRequest state event =
        match event with
        | HolidayCreated holiday -> PendingValidation holiday
        | HolidayValidated holiday -> Validated holiday
        | HolidayCancelPending holiday -> PendingCancelation holiday
        | HolidayDenyCancel holiday -> DeniedCancelation holiday

    let evolveUserRequests (userRequests: UserHolidaysState) (event: HolidayEvent) =
        let holidayState = defaultArg (Map.tryFind event.Request.HolidayId userRequests) NotCreated
        let newHolidayState = evolveRequest holidayState event
        userRequests.Add (event.Request.HolidayId, newHolidayState)

    let overlapsWith (holiday1:TimeOffHoliday) (holiday2:TimeOffHoliday) =
        match (holiday1, holiday2) with
        | (h1, h2) when h1 |> TheyCanBothTakeHolydayWhen h2 -> true
        | _ -> false
        
        // TODO: write a function that checks if 2 holidays overlap
    let overlapsWithAnyRequest (otherRequests: TimeOffHoliday seq) holiday =
         Seq.exists (fun req -> overlapsWith req holiday) otherRequests

    let createRequest today activeUserRequests holiday =
        if holiday |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping holiday"
        elif holiday.Start.Date <= today then
            Error "The holiday starts in the past"
        else
            Ok [HolidayCreated holiday]

    let validateHoliday holidayState =
        match holidayState with
        | PendingValidation holiday -> Ok [HolidayValidated holiday]
        | _ ->  Error "Holiday cannot be validated"
    
    let askCancelHoliday holidayState =
        match holidayState with 
        | Validated holiday -> Ok [HolidayCancelPending holiday]
        | _ -> Error "Holiday cannot be canceled"
    
    let denyCancelHoliday holidayState =
        match holidayState with
        | PendingCancelation holiday -> Ok [HolidayDenyCancel holiday]
        | _ ->  Error "Holiday cannot be denied of his cancelation"

    let decide  (today: DateTime) (userRequests: UserHolidaysState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | AskHolidayTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest today activeUserRequests request
            
            | AskCancelHoliday (_, holidayId) -> 
                let holidayState = defaultArg (userRequests.TryFind holidayId) NotCreated
                askCancelHoliday holidayState

            | DenyCancelHoliday (_, holidayId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind holidayId) NotCreated
                    denyCancelHoliday requestState

            | ValidateHoliday (_, holidayId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind holidayId) NotCreated
                    validateHoliday requestState
