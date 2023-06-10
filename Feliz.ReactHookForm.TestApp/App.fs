module Feliz.ReactHookForm.TestApp

open System
open Fable.Core
open Feliz
open Browser.Dom


type FormValues = {|
    name: string
    age: int
|}

let formValues = Unchecked.defaultof<FormValues>

[<Erase>]
module FormValues =
    let inline create name age : FormValues = {| name = name; age = age |}
    let inline createEmpty () = create "" 0


[<ReactComponent>]
let NameInput () =
    let form = ReactHookForm.useFormContext<FormValues>()
    let { field = field } as c =
        ReactHookForm.useController<FormValues, string> [
            rhfController.name (fun (x: FormValues) -> x.name)
            rhfController.control form.control
        ]

    Html.input [
        prop.value field.value
        prop.onChange field.onChange
        prop.onBlur (fun _ -> field.onBlur ())
        prop.name field.name
        prop.placeholder (nameof formValues.name)
    ]


[<ReactComponent>]
let AgeInput () =
    let form = ReactHookForm.useFormContext<FormValues>()
    let { field = field } as c =
        ReactHookForm.useController<FormValues, int> [
            rhfController.control form.control
            rhfController.name (fun (x: FormValues) -> x.age)
        ]

    Html.input [
        prop.type'.number
        prop.value field.value
        prop.onChange field.onChange
        prop.onBlur (fun _ -> field.onBlur ())
        prop.name field.name
        prop.placeholder (nameof formValues.age)
    ]


[<ReactComponent>]
let TestForm () =
    let form =
        ReactHookForm.useForm<FormValues> [
            rhfForm.mode.onBlur
            rhfForm.defaultValues (FormValues.create "John Doe" 18)
        ]

    let onSubmit = React.useMemo(fun () ->
        let submit (x: FormValues) = JS.console.log("Submitted!", x)
        form.handleSubmit submit ignore
    , [| form.handleSubmit |])

    FormProvider form [
        Html.form [
            prop.onSubmit onSubmit
            prop.children [
                NameInput ()
                AgeInput ()
                Html.button [
                    prop.type'.submit
                    prop.text "Submit"
                ]
            ]
        ]
    ]


[<ReactComponent>]
let App () =
    TestForm ()

let root = ReactDOM.createRoot (document.getElementById "fable-app")
root.render (App ())
