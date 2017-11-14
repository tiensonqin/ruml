type state = {
  id: int;
  atoms: (int, int) Atom.t list ref
}

let id = ref 0

let state = { id = (!id);
              atoms = (ref []) }

(* TODO: support old browsers *)
external requestAnimationFrame : (unit -> unit) -> unit = "" [@@bs.val]

let renderQueue = ref []

(* TODO: filter unmounted components *)
let renderAll queue =
  List.iter (fun comp ->
      (Obj.magic comp)##prototype##forceUpdate ()) queue

let render () =
  let queue = !renderQueue in
  renderQueue := [];
  renderAll queue

let requestRender component =
  if List.length !renderQueue = 0 then
    requestAnimationFrame render;
  renderQueue := component :: !renderQueue

let make componentName renderFn =
  let component = ReasonReact.reducerComponent componentName in
  {component with
   initialState = (fun ()  -> state);

   reducer = (fun () _ -> ReasonReact.NoUpdate);

   willUnmount = (fun { state } ->
       List.iter (fun atom  -> Atom.removeWatch atom state.id)
         (!(state.atoms)));

   render = (fun { state } ->
       let dom = renderFn () in
       List.iter (fun atom  -> Atom.addWatch atom state.id
                     (fun _ _ _ _ ->
                        Js.log component;
                        requestRender component.reactClassInternal;
                     )) !(state.atoms);
       dom
     )}

let react atom =
  state.atoms := atom :: !(state.atoms);
  Atom.deref atom
