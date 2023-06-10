module [<AutoOpen>] Feliz.ReactHookForm.Bindings

open System
open Fable.Core
open Fable.Core.JsInterop
open Feliz


type Regex = System.Text.RegularExpressions.Regex

type Control<'T> = { register: obj; unregister: obj }


type ValidationRule<'TValidationValue> = {|
    value: 'TValidationValue;
    message: string
|}

module ValidationRule =
    let inline create value message : ValidationRule<'ValidationValue> =
        {| value = value; message = message |}

type IRhfFormProp = interface end
type IRhfControllerProp<'FormValues, 'Value> = interface end
type IRhfRule = interface end
type IRhfFormResetProp<'FormValues> = interface end

let inline rhfFormProp (name: string) (value: 'T) : IRhfFormProp = unbox (name, value)
let inline rhfControllerProp<'FormValues, 'Value> (name: string) (value: obj) : IRhfControllerProp<'FormValues, 'Value> = unbox (name, value)
let inline rhfRule (name: string) (value: 'T) : IRhfRule = unbox (name, value)
let inline rhfFormResetProp<'FormValues> (name: string) (value: obj) : IRhfFormResetProp<'FormValues> = unbox (name, value)

type private SubmitHandlerPromise<'T> = 'T -> JS.Promise<unit>
type private SubmitErrorHandlerPromise = obj -> JS.Promise<unit>
type RuleValidateOption<'T> = 'T -> Option<string>
type RuleValidateResult<'T> = 'T -> Result<'T, string>
type RuleValidateAsyncResult<'T> = 'T -> Async<Result<'T, string>>
type RuleValidatePromiseOption<'T> = 'T -> JS.Promise<string option>

type FormState<'T> =
    abstract isDirty: bool
    abstract isSubmitted: bool
    abstract isSubmitSuccessful: bool
    abstract submitCount: int
    abstract isSubmitting: bool
    abstract isValidating: bool
    abstract isValid: bool


type ControllerValidationError =
    abstract message: string


type internal ControllerRenderPropsInternal<'F> =
    abstract name: string
    abstract value: 'F
    abstract onChange: (Browser.Types.Event -> 'F -> unit)
    abstract onBlur: (unit -> unit)

type internal ControllerFieldStateInternal =
    abstract invalid: bool
    abstract isTouched: bool
    abstract isDirty: bool
    abstract error: ControllerValidationError option

type internal UseFormReturnInternal<'T> =
    abstract control: Control<'T>
    abstract handleSubmit: SubmitHandlerPromise<'T> -> SubmitErrorHandlerPromise -> Func<Browser.Types.Event, unit>
    abstract reset: obj -> obj -> unit
    abstract formState: FormState<'T>
    abstract getValues: obj

type internal UseControllerReturnInternal<'F> =
    abstract field: ControllerRenderPropsInternal<'F>
    abstract fieldState: ControllerFieldStateInternal


let inline internal FormProvider (props: UseFormReturnInternal<'T>) children : ReactElement =
    Fable.React.Helpers.ofImport "FormProvider" "react-hook-form" props children

let inline internal useFormContext<'T> () : UseFormReturnInternal<'T> =
    (import "useFormContext" "react-hook-form") ()

let inline internal useForm<'FormValues> (props: seq<IRhfFormProp>) : UseFormReturnInternal<'FormValues> =
    (import "useForm" "react-hook-form") (createObj !!props)

let inline internal useController<'FormValues, 'Value> (props: seq<IRhfControllerProp<'FormValues, 'Value>>) : UseControllerReturnInternal<'Value> =
    (import "useController" "react-hook-form") (createObj !!props)
