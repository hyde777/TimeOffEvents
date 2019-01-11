module Client.Holidays.State

open Elmish
open Fable.Core.JsInterop
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Thoth.Json

open TimeOff
open TimeOff.AuthTypes
open Client
open Client.Holidays.Types
open System

let createNewHolidayOnApi model =
   promise {
      if String.IsNullOrEmpty model.StartDate then return! failwithf "You need to fill in a start date."
      if String.IsNullOrEmpty model.EndDate then return! failwithf "You need to fill in a end date."

      let startDate = DateTime.Parse model.StartDate
      let endDate = DateTime.Parse model.EndDate

      let timeOff : TimeOffHoliday = {
        UserId = ""
        HolidayId = Guid.NewGuid()
        Start = { Date = startDate; HalfDay = AM}
        End = {Date = endDate; HalfDay = AM}
      }
      let body = Encode.Auto.toString(2, timeOff)

      let props = 
          [ RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders [
              HttpRequestHeaders.ContentType "application/json" ]
            RequestProperties.Body !^body ]
      
      try
          let! response = Fetch.fetch ServerUrls.NewHoliday props

          if not response.Ok then
              return! failwithf "Error: %d" response.Status
          else
              return printfn "Your time off got created"
      with
      | _ -> return! failwithf "Could not authenticate user."
   }

let init user =
  { 
    StartDate = Date.Format.localFormat Date.Local.french "dd/MM/yyyy" DateTime.Today
    StartHalfDay = AM
    EndDate = Date.Format.localFormat Date.Local.french "dd/MM/yyyy" DateTime.Today
    EndHalfDay = PM
    Info = ""
  }, Cmd.none

let update msg model =
  match msg with
  | SetStartDate startdate -> 
    { model with StartDate = startdate } , Cmd.none
  | SetStartHalfDay startHalfDay -> 
    { model with StartHalfDay = startHalfDay}, Cmd.none
  | SetEndDate endDate -> 
    { model with EndDate = endDate } , Cmd.none
  | SetEndHalfDay endHalfDay -> 
    { model with EndHalfDay = endHalfDay}, Cmd.none
  | ClickCreateTimeOff ->
    model, Cmd.ofPromise createNewHolidayOnApi model NetworkOk NetworkError
  | NetworkOk _ ->
    printfn "Your TimeOff got created" 
    model, Cmd.none
  | NetworkError error ->
    printfn "[NewHoliday.State][Network error] %s" error.Message
    model, Cmd.none