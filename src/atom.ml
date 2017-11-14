type meta

type ('a, 'b) t = {
  state: 'a ref;
  meta: meta option;
  validator: 'a validator option;
  watches: ('b, ('a, 'b) fn) Hashtbl.t;
}
and 'a validator = 'a -> bool
and ('a, 'b) fn =
  'b -> ('a, 'b) t -> 'a -> 'a -> unit

let equal t1 t2 =
  !(t1.state) = !(t2.state)

let deref t =
  !(t.state)

let meta t =
  t.meta

let notifyWatches t oldV newV =
  Hashtbl.iter (fun key fn -> fn key t oldV newV) t.watches

let atom ?meta ?validator state =
  { state = ref(state);
    meta = meta;
    validator = validator;
    watches = Hashtbl.create(10)
  }

let addWatch t key fn =
  Hashtbl.add t.watches key fn

let removeWatch t key =
  Hashtbl.remove t.watches key

let reset t newV =
  match t.validator with
  | None ->
    begin
      let oldV = !(t.state) in
      t.state := newV;
      notifyWatches t oldV newV;
      (Result.Ok t.state)
    end
  | Some f ->
    if (f newV) then begin
      let oldV = !(t.state) in
      t.state := newV;
      notifyWatches t oldV newV;
      (Result.Ok t.state)
    end else
      (Result.Error "Validate failed.")

let swap t fn =
  reset t (fn !(t.state))
