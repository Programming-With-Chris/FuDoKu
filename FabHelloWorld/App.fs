namespace FabHelloWorld

open Xamarin.Forms
open Fabulous.XamarinForms

open type View

module App =
    type Model = { Count: int }

    type Msg =
        | Increment
        | Decrement

    let init () = { Count = 0 }

    let update msg model =
        match msg with
        | Increment -> { model with Count = model.Count + 1 }
        | Decrement -> { model with Count = model.Count - 1 }

    let view model =
        Application(
            ContentPage(
                "FabHelloWorld",
                VStack() {
                    Label("Fudoku")
                        .font(namedSize = NamedSize.Title)
                        .centerTextHorizontal()

                    (VStack() {
                        Label($"Count is {model.Count}").centerTextHorizontal()
                        Path(
                             RectangleGeometry(new Rect(0.0,0.0,100.0,100.0))
                             ).center()
                             .stroke(SolidColorBrush(Color.Black))

                        Button("Increment", Increment)
                        Button("Decrement", Decrement)
                    })
                        .centerVertical(expand = true)



                }
            )
        )

    let program = Program.stateful init update view
