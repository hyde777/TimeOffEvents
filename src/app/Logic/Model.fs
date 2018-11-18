namespace TimeOff

open System
open EventStorage
open HolidayTools
// Then our commands
type Command =
    | RequestTimeOff of TimeOffHoliday
    | ValidateRequest of UserId * Guid with
    member this.UserId =
        match this with
        | RequestTimeOff holiday -> holiday.UserId
        | ValidateRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffHoliday
    | RequestValidated of TimeOffHoliday with
    member this.Request =
        match this with
        | RequestCreated holiday -> holiday
        | RequestValidated holiday -> holiday

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffHoliday
        | Validated of TimeOffHoliday with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation holiday
            | Validated holiday -> holiday
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | Validated _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated holiday -> PendingValidation holiday
        | RequestValidated holiday -> Validated holiday

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let holidayState = defaultArg (Map.tryFind event.Request.HolidayId userRequests) NotCreated
        let newRequestState = evolveRequest holidayState event
        userRequests.Add (event.Request.HolidayId, newRequestState)

    let overlapsWith (holiday1:TimeOffHoliday) (holiday2:TimeOffHoliday) =
        match (holiday1, holiday2) with
        | (h1, h2) when h1 |> isTheSameThanTheOther h2 -> true
        | _ -> false
        
        // Ou est la Motherfucking doc
        // TODO: write a function that checks if 2 holidays overlap

    let overlapsWithAnyRequest (otherRequests: TimeOffHoliday seq) holiday =
         Seq.exists (fun req -> overlapsWith req holiday) otherRequests

        // false //TODO: write this function using overlapsWith

    let createRequest activeUserRequests  holiday =
        if holiday |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping holiday"
        // This DateTime.Today must go away!
        elif holiday.Start.Date <= DateTime.Today then
            Error "The holiday starts in the past"
        else
            Ok [RequestCreated holiday]

    let validateRequest holidayState =
        match holidayState with
        | PendingValidation holiday ->
            Ok [RequestValidated holiday]
        | _ ->
            Error "Request cannot be validated"

    let decide (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeUserRequests request

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState
