module Client.Holidays.Types

open TimeOff
open TimeOff.AuthTypes

type Model = {
  StartDate : string
  StartHalfDay : HalfDay
}
  
type Msg =
  | SetStartDate of string
  | SetStartHalfDay of HalfDay
  | ClickCreateTimeOff
