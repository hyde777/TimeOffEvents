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
    | CancelHoliday of UserId * Guid
    | RefuseHoliday of UserId * Guid
    with member this.UserId =
            match this with
            | AskHolidayTimeOff holiday -> holiday.UserId
            | ValidateHoliday (userId, _) -> userId
            | AskCancelHoliday (userId, _) -> userId
            | DenyCancelHoliday (userId, _) -> userId
            | CancelHoliday (userId, _) -> userId
            | RefuseHoliday (userId, _) -> userId

// And our events (ce qu'on écoute)
type HolidayEvent =
    | HolidayCreated of TimeOffHoliday
    | HolidayRefused of TimeOffHoliday
    | HolidayValidated of TimeOffHoliday 
    | HolidayCancelPending of TimeOffHoliday
    | HolidayDenyCancel of TimeOffHoliday
    | HolidayCancel of TimeOffHoliday
    with member this.Request =
            match this with
            | HolidayCreated holiday -> holiday
            | HolidayRefused holiday -> holiday
            | HolidayValidated holiday -> holiday
            | HolidayCancelPending holiday -> holiday
            | HolidayDenyCancel holiday -> holiday
            | HolidayCancel holiday -> holiday

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =
    type HolidayState =
        | NotCreated
        | Refused of TimeOffHoliday
        | PendingValidation of TimeOffHoliday
        | Validated of TimeOffHoliday
        | PendingCancelation of TimeOffHoliday
        | DeniedCancelation of TimeOffHoliday
        | Canceled of TimeOffHoliday with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | Refused holiday
            | PendingValidation holiday
            | Validated holiday 
            | PendingCancelation holiday
            | DeniedCancelation holiday
            | Canceled holiday -> holiday
        member this.IsActive =
            match this with
            | NotCreated
            | Refused _
            | Canceled _ -> false
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
        | HolidayCancel holiday -> Canceled holiday

    let evolveUserRequests (userRequests: UserHolidaysState) (event: HolidayEvent) =
        let holidayState = defaultArg (Map.tryFind event.Request.HolidayId userRequests) NotCreated
        let newHolidayState = evolveRequest holidayState event
        userRequests.Add (event.Request.HolidayId, newHolidayState)

    let overlapsWith (holiday1:TimeOffHoliday) (holiday2:TimeOffHoliday) =
        match holiday1, holiday2 with
        | hol1, hol2 when isBetweenBoundaryOf hol1 hol2 -> true
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
    
    let refuseHoliday holidayState =
        match holidayState with
        | PendingValidation holiday -> Ok [HolidayRefused holiday]
        | _ ->  Error "Holiday cannot be refused"

    let askCancelHoliday holidayState today =
        match holidayState with 
        | PendingValidation holiday
        | Validated holiday when holiday.Start.Date > today -> Ok [HolidayCancelPending holiday]
        | _ -> Error "Holiday cannot be ask to be canceled"
    
    let denyCancelHoliday holidayState =
        match holidayState with
        | PendingCancelation holiday -> Ok [HolidayDenyCancel holiday]
        | _ ->  Error "Holiday cannot be denied of his cancelation"
    
    let cancelHolidayByUser holidayState today =
        match holidayState with
        | PendingValidation holiday
        | Validated holiday when holiday.Start.Date > today -> Ok [HolidayCancel holiday]
        | _ ->  Error "Holiday cannot be canceled"

    let cancelHolidayByManager holidayState =
        match holidayState with
        | PendingValidation holiday
        | Validated holiday 
        | DeniedCancelation holiday
        | PendingCancelation holiday -> Ok [HolidayCancel holiday]
        | _ ->  Error "Holiday cannot be canceled"

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

            | ValidateHoliday (_, holidayId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind holidayId) NotCreated
                    validateHoliday requestState

            | RefuseHoliday (_, holidayId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind holidayId) NotCreated
                    refuseHoliday requestState

            | AskCancelHoliday (_, holidayId) -> 
                let holidayState = defaultArg (userRequests.TryFind holidayId) NotCreated
                askCancelHoliday holidayState today

            | DenyCancelHoliday (_, holidayId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind holidayId) NotCreated
                    denyCancelHoliday requestState

            | CancelHoliday (_, holidayId) ->
                if user <> Manager then
                    let requestState = defaultArg (userRequests.TryFind holidayId) NotCreated
                    cancelHolidayByUser requestState today
                else
                    let requestState = defaultArg (userRequests.TryFind holidayId) NotCreated
                    cancelHolidayByManager requestState
