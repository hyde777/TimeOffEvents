module ServerCode.App

open TimeOff
open Storage.Events

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.Serialization.Json
open Giraffe.HttpStatusCodeHandlers.RequestErrors
open FSharp.Control.Tasks
open Thoth.Json.Giraffe

// ---------------------------------
// Handlers
// ---------------------------------

module HttpHandlers =

    open Microsoft.AspNetCore.Http

    [<CLIMutable>]
    type UserAndRequestId = {
        UserId: UserId
        RequestId: Guid
    }

    let requestTimeOff (handleCommand: Command -> Result<HolidayEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! timeOffRequest = ctx.BindJsonAsync<TimeOffHoliday>()
                let command = AskHolidayTimeOff timeOffRequest
                let result = handleCommand command
                match result with
                | Ok _ -> return! json timeOffRequest next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let validateRequest (handleCommand: Command -> Result<HolidayEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = ValidateHoliday (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [HolidayValidated timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }
    
    let refuseRequest (handleCommand: Command -> Result<HolidayEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = RefuseHoliday (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [HolidayRefused timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let askCancelationRequest (handleCommand: Command -> Result<HolidayEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = AskCancelHoliday (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [HolidayCancelPending timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let cancelRequest (handleCommand: Command -> Result<HolidayEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = CancelHoliday (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [HolidayCancel timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }
    
    let denyCancelRequest (handleCommand: Command -> Result<HolidayEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = DenyCancelHoliday (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [HolidayDenyCancel timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let getUserBalance (authentifiedUser: User) (userName: string) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let balance : UserVacationBalance = {
                  UserName = userName
                  BalanceYear = DateTime.Today.Year
                  CarriedOver = 0.0
                  PortionAccruedToDate = 10.0
                  TakenToDate = 0.0
                  CurrentBalance = 10.
                }
                return! json balance next ctx
            }

// ---------------------------------
// Web app
// ---------------------------------

let webApp (eventStore: IStore<UserId, HolidayEvent>) =
    let handleCommand (user: User) (command: Command) =
        let userId = command.UserId

        let eventStream = eventStore.GetStream(userId)
        let state = eventStream.ReadAll() |> Seq.fold Logic.evolveUserRequests Map.empty

        // Decide how to handle the command
        let result = Logic.decide DateTime.Today state user command

        // Save events in case of success
        match result with
        | Ok events -> eventStream.Append(events)
        | _ -> ()

        // Finally, return the result
        result
        
    choose [
        subRoute "/api"
            (choose [
                route "/users/login/" >=> POST >=> Auth.Handlers.login
                subRoute "/timeoff"
                    (Auth.Handlers.requiresJwtTokenForAPI (fun user ->
                        choose [
                            POST >=>
                                (choose [
                                    routex "/request/?" >=> HttpHandlers.requestTimeOff (handleCommand user)
                                    routex "/validate-request/?" >=> HttpHandlers.validateRequest (handleCommand user)
                                    routex "/refuse-request/?" >=> HttpHandlers.refuseRequest (handleCommand user)
                                    routex "/ask-cancel-request/?" >=> HttpHandlers.askCancelationRequest (handleCommand user)
                                    routex "/cancel-request/?" >=> HttpHandlers.cancelRequest (handleCommand user)
                                    routex "/deny-cancel-request/?" >=> HttpHandlers.denyCancelRequest (handleCommand user)
                                ])
                            GET >=> routef "/user-balance/%s" (HttpHandlers.getUserBalance user)
                        ]
                    ))
            ])
        RequestErrors.NOT_FOUND "Not found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex: Exception) (logger: ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder: CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (eventStore: IStore<UserId, HolidayEvent>) (app: IApplicationBuilder) =
    let webApp = webApp eventStore
    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    (match env.IsDevelopment() with
    | true -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services: IServiceCollection) =
    services.AddCors() |> ignore
    services.AddGiraffe() |> ignore
    services.AddSingleton<IJsonSerializer>(ThothSerializer()) |> ignore

let configureLogging (builder: ILoggingBuilder) =
    let filter (l: LogLevel) = l.Equals LogLevel.Error
    builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()

    //let eventStore = InMemoryStore.Create<UserId, RequestEvent>()
    let storagePath = System.IO.Path.Combine(contentRoot, "../../../.storage", "userRequests")
    let eventStore = FileSystemStore.Create<UserId, HolidayEvent>(storagePath, id)

    let webRoot = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder>(configureApp eventStore))
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0