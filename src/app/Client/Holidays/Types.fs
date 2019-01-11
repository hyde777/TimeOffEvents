module Client.Holidays.Types

open TimeOff
open TimeOff.AuthTypes

type Model = {
  StartDate : string
  EndDate : string
  StartHalfDay : HalfDay
  EndHalfDay : HalfDay
  Info: string
}
  
type Msg =
  | SetStartDate of string
  | SetEndDate of string
  | SetStartHalfDay of HalfDay
  | SetEndHalfDay of HalfDay
  | ClickCreateTimeOff
  | NetworkOk of unit
  | NetworkError of exn
