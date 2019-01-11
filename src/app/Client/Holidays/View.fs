module Client.Holidays.View

open System
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome

open Types

let root (model:Model) dispatch =
  let buttonActive = if String.IsNullOrEmpty model.StartDate then Button.Disabled true else Button.Color IsPrimary

  form [ ] [
      Field.div [ ]
        [ Label.label [ ]
            [ str "Start Date" ]
          Control.div [ Control.HasIconLeft ]
            [ Input.input [ Input.Type Input.Text
                            Input.Id "StartDate"
                            Input.Placeholder "dd/MM/yyyy"
                            Input.DefaultValue model.StartDate
                            Input.Props [
                              OnChange (fun ev -> dispatch (SetStartDate !!ev.target?value))
                              AutoFocus true ] ]
              Icon.faIcon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.icon Fa.I.User ] ] ]

      Field.div [ ]
        [ Label.label [ ]
            [ str "Start HalfDay" ]
          Control.div [ Control.HasIconLeft ]
            [ Input.input [ Input.Type Input.Text
                            Input.Id "StartHalfday"
                            Input.Placeholder "AM or PM"
                            Input.Props [
                              OnChange (fun ev -> dispatch (SetStartHalfDay !!ev.target?value))
                              AutoFocus true ] ]
              Icon.faIcon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.icon Fa.I.User ] ] ]

      Field.div [ ]
        [ Label.label [ ]
            [ str "End Date" ]
          Control.div [ Control.HasIconLeft ]
            [ Input.input [ Input.Type Input.Text
                            Input.Id "EndDate"
                            Input.Placeholder "dd/MM/yyyy"
                            Input.DefaultValue model.EndDate
                            Input.Props [
                              OnChange (fun ev -> dispatch (SetEndDate !!ev.target?value))
                              AutoFocus true ] ]
              Icon.faIcon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.icon Fa.I.User ] ] ]

      Field.div [ ]
        [ Label.label [ ]
            [ str "End Halfday" ]
          Control.div [ Control.HasIconLeft ]
            [ Input.input [ Input.Type Input.Text
                            Input.Id "EndHalfday"
                            Input.Placeholder "AM or PM"
                            Input.Props [
                              OnChange (fun ev -> dispatch (SetEndHalfDay !!ev.target?value))
                              AutoFocus true ] ]
              Icon.faIcon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.icon Fa.I.User ] ] ]

      div [ ClassName "text-center" ] [
          Button.a [buttonActive; Button.OnClick  (fun _ -> dispatch ClickCreateTimeOff)] [ str "Create TimeOff" ]
      ]
    ]    