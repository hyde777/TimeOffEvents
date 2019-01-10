module Client.Holidays.State

open Elmish
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Thoth.Json

open TimeOff
open TimeOff.AuthTypes
open Client
open Client.Holidays.Types
open System

let init user =
  { StartDate = Date.Format.localFormat Date.Local.french "dd/MM/yyyy" DateTime.Today; StartHalfDay = AM}, Cmd.none

let update msg model =
  match msg with
  | SetStartDate startdate -> 
    { model with StartDate = startdate } , Cmd.none
  | SetStartHalfDay startHalfDay -> 
    { model with StartHalfDay = startHalfDay}, Cmd.none