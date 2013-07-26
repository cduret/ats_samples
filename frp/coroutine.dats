staload _(*anonymous*) = "prelude/DATS/list.dats"

datatype coroutine_dt (i: t@ype, o: t@ype) = Coroutine(i, o) of (i -<cloref1> (o, coroutine_dt(i,o)))

typedef event_t (a: t@ype) = List(a)
extern castfn events_of_list {a: t@ype} (l: List a):<> event_t a

extern fun{a,b: t@ype} co_run(co: coroutine_dt(a, b), x: a): (b,coroutine_dt(a,b))
extern fun{a,b: t@ype} co_run_seq(co: coroutine_dt(a, b), seq: List a): List b

// functor
extern fun{a,b,c: t@ype} co_fmap(co: coroutine_dt(a,b), f: b -> c): coroutine_dt(a,c)
// applicative functor
extern fun{a,b: t@ype} co_app_pure(x: b): coroutine_dt(a,b)
extern fun{a,b,c: t@ype} co_app_map(cof: coroutine_dt(a, b -> c), cox: coroutine_dt(a, b)): coroutine_dt(a,c)
// category
extern fun{a: t@ype} co_cat_id(): coroutine_dt(a,a)
extern fun{a,b,c: t@ype} co_cat_compose(cof: coroutine_dt(b, c), cog: coroutine_dt(a, b)): coroutine_dt(a, c)
// arrow
extern fun{a,b: t@ype} co_arr(f: a -<cloref1> b): coroutine_dt(a, b)
extern fun{a,b,c: t@ype} co_arr_first(co: coroutine_dt(a, b)): coroutine_dt((a,c), (b,c))
extern fun{a,b,c: t@ype} co_arr_second(co: coroutine_dt(a, b)): coroutine_dt((c,a), (c,b))
extern fun{a,b,c: t@ype} co_arr_bind(cof: coroutine_dt(a, b), cog: coroutine_dt(b, c)): coroutine_dt(a, c)
extern fun{a,b,c,d: t@ype} co_arr_combine(cof: coroutine_dt(a, b), cog: coroutine_dt(c, d)): coroutine_dt((a, c), (b, d))
extern fun{a,b,c: t@ype} co_arr_fanout(cof: coroutine_dt(a, b), cog: coroutine_dt(a, c)): coroutine_dt(a, (b,c))

extern fun{a, b: t@ype} co_arr_loop(co: coroutine_dt((a,b), b), seed: b): coroutine_dt(a, b)

// frp
extern fun{a,b: t@ype} co_scan(f: (a, b) -> a, i: a): coroutine_dt(b, a)
extern fun{a: t@ype} co_with_previous(first: a): coroutine_dt(a, (a,a))
extern fun{a: t@ype} co_delay(x: a): coroutine_dt(a,a)
extern fun{a: t@ype} co_integrate(x: a): coroutine_dt(a, a)
extern fun{a, b, c: t@ype} co_zipwith(f: (a, b) -> c): coroutine_dt((a,b), c)
extern fun{a: t@ype} co_derivate(): coroutine_dt(a, a)
extern fun{a: t@ype} co_watch(f: a -> bool): coroutine_dt(a, event_t a)

// events
extern fun{a: t@ype} ev_no_events(e: event_t a): bool
extern fun{a: t@ype} ev_any_events(e: event_t a): bool
extern fun{a: t@ype} ev_last(e: event_t a): Option a
extern fun{a,b: t@ype} ev_map(e: event_t a, f: a -> b): event_t(b)
extern fun{a,b: t@ype} ev_map_clo(e: event_t a, f: a -<cloref> b): event_t(b)
extern fun{a: t@ype} ev_merge(e: event_t a, e': event_t(a)): event_t(a)
extern fun{a, b: t@ype} ev_fold(e: event_t a, f: (b, a) -> b, seed: b): b
extern fun{a: t@ype} ev_filter(e: event_t a, f: a -> bool): event_t(a)
#define NO_EVENTS list_nil()
extern fun{a: t@ype} ev_tag(x: a): event_t(a)

// event FRP
extern fun{a, b: t@ype} co_arr_ev_map(f: a -> b): coroutine_dt(event_t a, event_t b)
extern fun{a, b: t@ype} co_arr_ev_map_clo(f: a -<cloref> b): coroutine_dt(event_t a, event_t b)
extern fun{a: t@ype} co_arr_ev_filter(f: a -> bool): coroutine_dt(event_t a, event_t a)
extern fun{a, b: t@ype} co_arr_ev_const(x: b): coroutine_dt(event_t a, event_t b)
extern fun{a: t@ype} co_arr_ev_zip(): coroutine_dt((event_t a, event_t a), event_t a)
extern fun{a, b: t@ype} co_arr_ev_scan(f: (b, a) -> b, seed: b): coroutine_dt(event_t a, b)

implement{a,b} co_run(co, x) = let
  val Coroutine (f) = co
  val f = f: a -<cloref1> (b, coroutine_dt(a,b))
in
  f x
end

implement{a, b} co_run_seq(co, seq) = if( length(seq) = 0 ) then
  list_nil()
else
  case+ seq of list_cons(x, y) => let
    val res = co_run(co, x)
  in
    list_cons(res.0, co_run_seq(res.1, y))
  end

// functor
implement {a,b,c} co_fmap(co, f) = Coroutine(lam i => let
  val (o, co') = co_run(co, i)
in
  (f o, co_fmap(co', f))
end )

// applicative functor
implement{a,b} co_app_pure(x) = Coroutine(lam _ => (x, co_app_pure(x)))
implement{a,b, c} co_app_map(cof, cox) = Coroutine(lam i => let
  val (f, cof') = co_run(cof, i)
  val (x, cox') = co_run(cox, i)
in
  (f x, co_app_map(cof', cox'))
end )

// category
implement{a} co_cat_id() = Coroutine(lam i => (i, co_cat_id()))
implement{a, b, c} co_cat_compose(cof, cog) = Coroutine(lam i => let
  val (x, cog') = co_run(cog, i)
  val (y, cof') = co_run(cof, x)
in
  (y, co_cat_compose(cof', cog'))
end )

// arrow
implement{a,b} co_arr(f) = Coroutine(lam i => (f i, co_arr f))
implement{a,b,c} co_arr_first(co) = Coroutine(clo) where {
  val clo = lam (p: (a, c)): ((b,c), coroutine_dt((a,c), (b,c))) =<cloref1> let
    val (i, z) = p
    val (o, co') = co_run(co, i)
  in
    ((o, z), co_arr_first co')
  end
}
implement{a,b,c} co_arr_second(co) = Coroutine(clo) where {
  val clo = lam (p: (c, a)): ((c,b), coroutine_dt((c,a), (c,b))) =<cloref1> let
    val (z, i) = p
    val (o, co') = co_run(co, i)
  in
    ((z, o), co_arr_second co')
  end
}
implement{a,b,c} co_arr_bind(cof, cog) = co_cat_compose(cog, cof)
implement{a,b,c,d} co_arr_combine(cof, cog) = co_arr_bind( co_arr_first cof, co_arr_second cog )
implement{a,b,c} co_arr_fanout(cof, cog) = co_arr_bind(co_arr f, co_arr_combine(cof, cog) ) where {
  fun f (x: a):<cloref1> (a, a) = (x, x)
}
// loop
implement{a, b} co_arr_loop(co, seed) = let
  fun loop_aux(co: coroutine_dt((a, b), b), seed: b): coroutine_dt(a, b) = Coroutine( lam x => let
      val (y, co') = co_run(co, (x, seed))
    in
      (y, loop_aux(co', y))
    end )
in
  loop_aux(co, seed)
end

// frp
implement{a,b} co_scan(f, i) = Coroutine(lam x => step(i, x)) where {
  fun step(x: a, y: b):<cloref1> (a, coroutine_dt(b, a)) = let
    val a' = f(x, y)
  in (a', co_scan(f, a')) end
}
implement{a} co_with_previous(first) = Coroutine(lam i => ((i,first), step i)) where {
  fun step(old: a): coroutine_dt(a, (a, a)) = Coroutine(lam i => ((i, old), step(i)))
}
implement{a} co_delay(x) = co_arr_bind(co_with_previous x, co_arr(f)) where {
  fun f(p: (a, a)):<cloref1> a = let val (snd, _) = p in snd end
}
implement co_integrate<int>(x) = co_scan<int, int>(lam (x, y) => x + y, x)
implement co_integrate<float>(x) = co_scan<float, float>(lam (x, y) => x + y, x)
implement co_integrate<double>(x) = co_scan<double, double>(lam (x, y) => x + y, x)
implement{a,b,c} co_zipwith(f) = co_arr<(a,b),c>( lam x =<cloref1> f(x.0, x.1) )
implement co_derivate<int>() = co_arr_bind( co_with_previous<int>(0), 
                                            co_zipwith<int,int,int>(lam (x, y) => x - y) )
implement co_derivate<float>() = co_arr_bind( co_with_previous<float>(0.0f), 
                                            co_zipwith<float,float,float>(lam (x, y) => x - y) )
implement co_derivate<double>() = co_arr_bind( co_with_previous<double>(0.0), 
                                            co_zipwith<double,double,double>(lam (x, y) => x - y) )
implement{a} co_watch(f) = Coroutine( lam x => if( f x ) then (ev_tag<a>(x), co_watch f) else (NO_EVENTS, co_watch f) )

// events
implement{a} ev_no_events(e) = length(e) = 0
implement{a} ev_any_events(e) = length(e) > 0
// mettre un maybe pour faire un match exhaustif
implement{a} ev_last(e) = last e where {
  fun last(e: List a): Option a = case+ e of
    | list_cons(x, list_nil ()) => Some x
    | list_cons(x, y) => last y
    | list_nil() => None
}
extern castfn to_nl_list{a: t@ype}{n:nat} (x: list_vt(a, n)):<> list(a, n)
implement{a,b} ev_map(e, f) = to_nl_list( list_map_fun<a><b>(e, f) )
implement{a,b} ev_map_clo(e, f) = to_nl_list( list_map_cloref<a><b>(e, f) )
implement{a} ev_merge(e, e') = list_append(e, e')
implement{a,b} ev_fold(e, f, seed) = list_fold_left_fun(f, seed, e)
implement{a} ev_filter(e, f) = to_nl_list( list_filter_fun<a>(e, f) )
implement{a} ev_tag(x) = list_cons(x, list_nil())

// events FRP
implement{a,b} co_arr_ev_map(f) = co_arr<event_t(a), event_t(b)>(lam e => ev_map(e, f))
implement{a,b} co_arr_ev_map_clo(f) = co_arr<event_t(a), event_t(b)>(lam e => ev_map_clo(e, f))
implement{a} co_arr_ev_filter(f) = co_arr<event_t(a), event_t(a)>(lam e => ev_filter(e, f))
implement{a,b} co_arr_ev_const(x) = co_arr_ev_map_clo(lam _ =<cloref> x)
implement{a} co_arr_ev_zip() = co_zipwith<event_t a,event_t a,event_t a>(lam (x,y) =>  ev_merge(x, y))
implement{a,b} co_arr_ev_scan(f, seed) = Coroutine( lam e => step(seed, e) ) where {
  fun step(seed: b, e: event_t a):<cloref1> (b, coroutine_dt(event_t a, b)) = let
    val x = ev_fold<a,b>(e, f, seed)
  in
    (x, co_arr_ev_scan(f, seed))
  end
}

fun int_from(n: int): coroutine_dt(int,int) = Coroutine(lam _ => (n, int_from (n+1)))

fun print_list(l: List int): void = let
  val () = list_foreach_fun(l, f) where {
    fun f(n: int): void = printf("%i ", @(n))
  }
in
  print("\n")
end

fun print_list2(l: List @(int,int)): void = let
  val () = list_foreach_fun(l, f) where {
    fun f(n: (int,int)): void = printf("(%i,%i) ", @(n.0, n.1))
  }
in
  print("\n")
end

fun print_evts(e: event_t(int)): void = let
  val () = print("[ ")
  val () = list_foreach_fun(e, f) where {
    fun f(i: int): void = printf("%i ", @(i))
  }
in print("]") end

fun print_list_evts(l: List(event_t int)): void = let
  val () = list_foreach_fun(l, f) where {
    fun f(e: event_t(int)): void = if( list_length e > 0 ) then print_evts e
  }
in
  print("\n")
end

symintr >>> *** &&&
infixl (+) >>>
infixl (+) ***
infixl (+) &&&
overload >>> with co_arr_bind
overload *** with co_arr_combine
overload &&& with co_arr_fanout

extern fun{a: t@ype} arr_give_me(x: a): coroutine_dt(a, a)
implement{a} arr_give_me(x) = Coroutine( lam _ => (x, arr_give_me x) )

implement main(argc, argv) = let
  val coi = int_from 5
  val co_accsum = co_scan(add_int_int, 0)
  fun double_clo(x: int):<cloref1> int = x * 2
  fun double(x: int): int = x * 2
  val coi2 = co_fmap(coi, double)
  val coi3 = coi2 >>> co_accsum
  val () = print_list( co_run_seq(coi2, '[0,0,0,0,0]) )
  val () = print_list( co_run_seq(coi3, '[0,0,0,0,0]) )
  val co_pair = co_arr(lam (x:int): (int,int) => (x,x))
  val coi4 = (coi2 >>> co_pair) >>> co_arr_first(co_arr(double_clo))
  val () = print_list2( co_run_seq(coi4, '[0,0,0,0,0]) )
  val co_add1 = co_arr(lam (x:int): int => add_int_int(x,1))
  val co_mul2 = co_arr(lam (x:int): int => mul_int_int(x,2))
  val co_mul2' = co_arr<(int,int), int>(lam (z: (int, int)): int => z.0 * z.1)
  val coi5 = coi >>> (co_add1 &&& co_mul2)
  val () = print_list2( co_run_seq(coi5, '[0,0,0,0,0]) )
  // testing loop
  val () = print_list( co_run_seq((int_from 1) >>> co_arr_loop(co_mul2', 1), '[0,0,0,0,0,0,0]) )
  // testing integration
  val () = print_list( co_run_seq((int_from 1) >>> (co_integrate 0), '[0,0,0,0,0,0,0]) )
  // testing derivation
  val () = print_list( co_run_seq((int_from 1) >>> co_derivate (), '[0,0,0,0,0,0,0]) )
  // testing frp
  val () = print_list_evts( co_run_seq((int_from 1) >>> co_watch<int>(lam x => x <= 3), '[0,0,0,0,0,0,0]) )
  // events
  val (o, _) = co_run( arr_give_me<event_t int>(events_of_list '[1,2,3]) >>> (co_arr_ev_const 7) , NO_EVENTS)
  val () = print_evts o val () = print("\n")
  val (o', _) = co_run( arr_give_me<event_t int>(events_of_list '[1,2,3]) >>> (co_arr_ev_map double) , NO_EVENTS)
  val () = print_evts o' val () = print("\n")
  val (p, _) = co_run( arr_give_me<(int,int)>(@(1,2)) >>> co_zipwith<int,int,int>(lam (x,y) => x + y) , @(0,0))
  val () = printf("%i\n", @(p))
  val (p', _) = co_run( arr_give_me<(event_t int, event_t int)>(@(ev_tag 1, ev_tag 2)) >>> co_arr_ev_zip() , (NO_EVENTS, NO_EVENTS) )
  val () = print_evts p' val () = print("\n")
  val (q, _) = co_run( arr_give_me<event_t int>(events_of_list '[1,2,3]) >>> co_arr_ev_scan<int, int>(lam (_, x) => x, ~1) , NO_EVENTS)
  val () = printf("%i\n", @(q))
in
end
