#include "share/atspre_staload_basicdef.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload _(*anonymous*) = "prelude/DATS/list.dats"
staload _(*anonymous*) = "prelude/DATS/list_vt.dats"

vtypedef lcfun1 (a:vt0p, b:vt0p) = (a) -<lin,cloptr1> b
vtypedef cfun1 (a:vt0p, b:vt0p) = (a) -<cloptr1> b
vtypedef cfun2 (a:vt0p, b:vt0p, c: vt0p) = (a,b) -<cloptr1> c

//  coroutine signature
absvtype coroutine_vt (inp: vt0p, out: vt0p) = ptr

extern castfn coroutine_of_linc {a, b: vt0p}
(cf: lcfun1 (a, @(b, coroutine_vt (a, b)))):<> coroutine_vt (a, b)

extern castfn linc_of_coroutine {a, b:vt0p}
(co: coroutine_vt (a, b)):<> lcfun1 (a, @(b, coroutine_vt (a, b)))

// event signature
absviewtype event_vt (a: vt0p) = ptr

extern castfn event_of_list {a: vt0p}{n: nat} (l: list_vt(a, n)):<> event_vt a
extern castfn list_of_event {a: vt0p} (e: event_vt a):<> [n: nat] list_vt(a, n)

// coroutine definitions
extern fun{a,b: vt0p} co_run(co: coroutine_vt(a, b), x: a): (b,coroutine_vt(a,b))
extern fun{a,b: vt0p} co_run_seq{n: nat} (co: coroutine_vt(a, b), seq: list_vt(a, n)): list_vt(b, n)

// functor
extern fun{a,b,c: vt0p} co_fmap(co: coroutine_vt(a,b), f: cfun1(b, c)): coroutine_vt(a,c)

// arrow
extern fun{a,b: vt0p} co_arr(f: cfun1(a, b)): coroutine_vt(a, b)
extern fun{a,b,c: vt0p} co_arr_first(co: coroutine_vt(a, b)): coroutine_vt((a,c), (b,c))
extern fun{a,b,c: vt0p} co_arr_second(co: coroutine_vt(a, b)): coroutine_vt((c,a), (c,b))
extern fun{a,b,c: vt0p} co_arr_bind(cof: coroutine_vt(a, b), cog: coroutine_vt(b, c)): coroutine_vt(a, c)
extern fun{a,b,c,d: vt0p} co_arr_combine(cof: coroutine_vt(a, b), cog: coroutine_vt(c, d)): coroutine_vt((a, c), (b, d))
extern fun{a,b,c: vt0p} co_arr_fanout(cof: coroutine_vt(a, b), cog: coroutine_vt(a, c)): coroutine_vt(a, (b,c))
extern fun{a, b: vt0p} co_arr_loop(co: coroutine_vt((a,b), b), seed: b): coroutine_vt(a, b)

// to be defined by each resource
extern fun{a: vt0p} co_lresource_dup(x: a): (a,a)
extern fun{a: vt0p} co_lresource_free(x: a): void

// implementations of resources operations
implement co_lresource_dup<int>(x) = (x,x)
implement co_lresource_dup<float>(x) = (x,x)
implement co_lresource_dup<double>(x) = (x,x)

implement co_lresource_dup<event_vt int>(x) = let
  val l = list_of_event x
  val l' = list_vt_copy l
in (event_of_list l, event_of_list l') end

implement co_lresource_free<event_vt int>(x) = let
  val l = list_of_event x
in
  list_vt_free l
end

// frp
extern fun{a,b: vt0p} co_scan(f: cfun2(a,b,a), i: a): coroutine_vt(b, a)
extern fun{a: vt0p} co_with_previous(first: a): coroutine_vt(a, (a,a))
extern fun{a: vt0p} co_delay(x: a): coroutine_vt(a,a)
extern fun{a: vt0p} co_integrate(x: a): coroutine_vt(a, a)
extern fun{a: vt0p} co_derivate(): coroutine_vt(a, a)

extern fun{a: t@ype} co_watch(f: cfun1(a,bool)): coroutine_vt(a, event_vt(a))

// events
extern fun{a: t@ype} event_vt_tag(x: a): event_vt(a)
extern fun{a: t@ype} NO_EVENTS(): event_vt(a)
extern fun{a: t@ype} event_vt_no_events(e: event_vt(a)): (event_vt(a), bool)
extern fun{a: t@ype} event_vt_any_events(e: event_vt(a)): (event_vt(a), bool)
// zzz
extern fun{a: t@ype} event_vt_last(e: event_vt(a)): [b: bool] (event_vt(a), option_vt(a, b))

extern fun{a,b: t@ype} event_vt_map(e: event_vt a, f: !cfun1(a,b)): event_vt b
extern fun{a: t@ype} event_vt_merge(e: event_vt a, e': event_vt(a)): event_vt(a)
extern fun{a, b: t@ype} event_vt_fold(e: event_vt a, f: !cfun1((b, a),b), seed: b): b
extern fun{a: t@ype} event_vt_filter(e: event_vt a, f: !cfun1(a,bool)): event_vt(a)

// coroutine implementation

implement{a,b} co_run(co, x) = res where {
  val f = linc_of_coroutine{a, b}(co)
  val res = f x
  val () = cloptr_free ($UN.castvwtp0{cloptr0}(f))
}

implement{a,b} co_run_seq(co, seq) = case+ seq of
  | ~list_vt_nil () => let 
    extern castfn __leak {v:view} (pf: v):<> void
    val () = __leak(co) // !!!!!
  in
    list_vt_nil{b} ()
  end
  | ~list_vt_cons (x, xs) => let
    val (res, co') = co_run<a, b>(co, x)
  in
    list_vt_cons{b} (res, co_run_seq(co', xs))
  end

implement{a,b,c} co_fmap(co, f) = coroutine_of_linc{a, c}(llam x =<cloptr1> let
  val (x', co') = co_run<a, b>(co, x)
in
  (f(x'), co_fmap<a, b, c>(co', f))
end )

implement{a,b} co_arr(f) = coroutine_of_linc{a,b}(llam x =<cloptr1> (f x, co_arr(f)))

implement{a,b,c} co_arr_first(co) = coroutine_of_linc{(a,c), (b,c)}(llam (p) =<cloptr1> let
    val (i, z) = p
    val (o, co') = co_run(co, i)
  in
    ((o, z), co_arr_first co')
  end
)

implement{a,b,c} co_arr_second(co) = coroutine_of_linc{(c,a), (c,b)}(llam (p) =<cloptr1> let
    val (z, i) = p
    val (o, co') = co_run(co, i)
  in
    ((z, o), co_arr_second co')
  end
)
implement{a,b,c} co_arr_bind(cof, cog) = coroutine_of_linc{a,c}(llam i =<cloptr1> let
  val (x, cof') = co_run(cof, i)
  val (y, cog') = co_run(cog, x)
in
  (y, co_arr_bind<a,b,c>(cof', cog'))
end )

implement{a,b,c,d} co_arr_combine(cof, cog) = co_arr_bind( co_arr_first cof, co_arr_second cog )

implement{a,b,c} co_arr_fanout(cof, cog) = let
  fun f (x: a):<cloptr1> (a, a) = co_lresource_dup<a>(x)
  val r = co_arr_bind(co_arr f, co_arr_combine(cof, cog) ) 
  val () = cloptr_free($UN.castvwtp0{cloptr0}(f))
in
  r
end

// loop
implement{a, b} co_arr_loop(co, seed) = coroutine_of_linc{a, b}( llam x =<cloptr1> let
  val (y, co') = co_run<(a,b),b> (co, (x, seed))
  val (y', y'') = co_lresource_dup<b>(y)
in
  (y', co_arr_loop<a,b>(co', y''))
end )

// frp
implement{a,b} co_scan(f, i) = coroutine_of_linc{b, a}( llam x =<cloptr1> let
  val y = f(i, x)
  val (y', y'') = co_lresource_dup<a>(y)
in
  (y', co_scan<a,b>(f, y''))
end )
implement{a} co_with_previous(first) = coroutine_of_linc{a, (a,a)}(llam x =<cloptr1> let
  val (y,y') = co_lresource_dup<a>(x)
  fun step(old: a): coroutine_vt(a, (a, a)) = coroutine_of_linc{a, (a,a)}(llam x =<cloptr1> let
    val (y,y') = co_lresource_dup<a>(x)
  in ((y, old), step y') end)
in
  ((y,first), step y')
end)

implement{a} co_delay(x) = let
  fun{a: vt0p} f(p: (a, a)):<cloptr1> a = let val (fst, snd) = p val () = co_lresource_free<a>(fst) in snd end
  val r = co_arr_bind<a, (a,a), a>(co_with_previous<a>(x), co_arr<(a,a),a>(f))
  val () = cloptr_free($UN.castvwtp0{cloptr0}(f))
in r end

implement co_integrate<int>(x) = let
  fun f(x: int, y: int):<cloptr1> int = x + y
  val r = co_scan<int, int>(f, x)
  val () = cloptr_free($UN.castvwtp0{cloptr0}(f))
in r end
implement co_integrate<float>(x) = let
  fun f(x: float, y: float):<cloptr1> float = x + y
  val r = co_scan<float, float>(f, x)
  val () = cloptr_free($UN.castvwtp0{cloptr0}(f))
in r end
implement co_integrate<double>(x) = let
  fun f(x: double, y: double):<cloptr1> double = x + y
  val r = co_scan<double, double>(f, x)
  val () = cloptr_free($UN.castvwtp0{cloptr0}(f))
in r end
implement co_derivate<int>() = let
  fun f_deriv(x: (int, int)):<cloptr1> int = x.0 - x.1
  val r = co_arr_bind<int, (int,int), int>( co_with_previous<int>(0), 
                                            co_arr<(int,int), int>(f_deriv) )
  val () = cloptr_free($UN.castvwtp0{cloptr0}(f_deriv))
in r end
implement co_derivate<float>() = let
  fun f_deriv(x: (float, float)):<cloptr1> float = x.0 - x.1
  val r = co_arr_bind<float, (float,float), float>( co_with_previous<float>(0.0f), 
                                                    co_arr<(float,float), float>(f_deriv) )
  val () = cloptr_free($UN.castvwtp0{cloptr0}(f_deriv))
in r end
implement co_derivate<double>() = let
  fun f_deriv(x: (double, double)):<cloptr1> double = x.0 - x.1
  val r = co_arr_bind<double, (double,double), double>( co_with_previous<double>(0.0), 
                                                        co_arr<(double,double), double>(f_deriv) )
  val () = cloptr_free($UN.castvwtp0{cloptr0}(f_deriv))
in r end

implement{a} co_watch(f) = coroutine_of_linc{a, event_vt(a)}( llam x =<cloptr1> if( f x ) then 
  (event_vt_tag<a>(x), co_watch f)
else 
  (NO_EVENTS(), co_watch f))

// events
implement{a} event_vt_tag(x) = event_of_list(list_vt_cons{a} (x, list_vt_nil{a} ()))
implement{a} NO_EVENTS() = event_of_list{a} (list_vt_nil{a} ())
implement{a} event_vt_no_events(e) = (e', len = 0) where {
  val l = list_of_event e
  val len = list_vt_length l
  val e' = event_of_list l
}
implement{a} event_vt_any_events(e) = (e', len > 0) where {
  val l = list_of_event e
  val len = list_vt_length l
  val e' = event_of_list l
}

implement{a} event_vt_last(e) = @(e', x) where {
  fun {a: t@ype} last {n:nat} (e: list_vt(a, n)): [b: bool] option_vt(a, b) = case+ e of
    | ~list_vt_cons (x, ~list_vt_nil()) => Some_vt{a} x
    | ~list_vt_cons (x, y) => last<a> y 
    | ~list_vt_nil () => None_vt{a} ()
  val l = list_of_event{a} e
  val l' = list_vt_copy<a> l
  val e' = event_of_list{a} l
  val x = last<a> l'
}
implement{a,b} event_vt_map(e,f) = let 
  val l = list_of_event e
  fun {a, b: t@ype} map_aux{n: nat} (l: list_vt(a, n), f: !cfun1(a,b)): list_vt(b, n) = case+ l of
    | ~list_vt_nil () => list_vt_nil{b} ()
    | ~list_vt_cons (x, xs) => list_vt_cons{b} (f x, map_aux<a,b>(xs, f))
in
  event_of_list{b}(map_aux<a,b>(l, f))
end
implement{a} event_vt_merge(e, e') = let
  val l = list_of_event e
  val l' = list_of_event e'
in
  event_of_list{a}(list_vt_append(l, l'))
end
implement{a,b} event_vt_fold(e, f, seed) = let
  val l = list_of_event e
  fun {a, b: t@ype} fold_aux{n: nat} (l: list_vt(a, n), f: !cfun1((b,a),b), seed: b): b = case+ l of
    | ~list_vt_nil () => seed
    | ~list_vt_cons (x, xs) => let
      val h = fold_aux<a,b> (xs, f, seed)
    in  f @(h, x) end
in
  fold_aux<a,b>(l, f, seed)
end
implement{a} event_vt_filter(e, f) = let
  val l = list_of_event e
  fun {a:t@ype} filter_aux{n: nat} (l: list_vt(a, n), f: !cfun1(a,bool)): [m: nat | m <= n] list_vt(a, m) = case+ l of
    | ~list_vt_nil () => list_vt_nil{a} ()
    | ~list_vt_cons (x, xs) => if f(x) then list_vt_cons{a} (x, filter_aux<a>(xs, f)) else
      filter_aux<a>(xs, f)
in
  event_of_list{a}(filter_aux<a>(l, f))
end

extern fun ints_from (x: int): coroutine_vt (unit, int)
implement ints_from(x) = coroutine_of_linc{unit,int}(llam _ =<cloptr1> (x, ints_from (x+1)))

extern fun{a:vt0p} print_resource(x: a): void
extern fun{a:vt0p} print_free_list{n: nat} (l: list_vt(a, n), nl: bool): void
extern fun{a,b:vt0p} print_free_list2{n: nat} (l: list_vt((a,b), n)): void

implement print_resource<int>(x) = print x
implement print_resource<float>(x) = print x

implement(a) print_resource<event_vt(a)> (x) = let
  val l = list_of_event{a} x
in
  print_free_list<a>(l, false)
end

implement{a} print_free_list(l, nl) = let
  fun{a:vt0p} print_free_list_aux{n: nat} (l: list_vt(a,n)): void = case+ l of
  | ~list_vt_nil () => ()
  | ~list_vt_cons (x, xs) => let
    val () = print_resource<a>(x)
    val () = print_free_list_aux<a> xs
  in end 
  val () = print_free_list_aux<a> l
in if nl then print_newline () end

implement{a,b} print_free_list2(l) = let
  fun{a,b:vt0p} print_free_list2_aux{n: nat} (l: list_vt((a,b), n)): void = case+ l of
    | ~list_vt_nil () => ()
    | ~list_vt_cons (x, xs) => let
      val () = print("[")
      val () = print_resource<a>(x.0)
      val () = print(", ")
      val () = print_resource<b>(x.1)
      val () = print("]")
      val () = print_free_list2_aux<a,b> xs
    in end
  val () = print_free_list2_aux<a,b> l
in print_newline() end

extern fun ints_evt_from (x: int): coroutine_vt (unit, event_vt int)
implement ints_evt_from(x) = coroutine_of_linc{unit,event_vt int}(llam _ => (event_vt_tag x, ints_evt_from (x+1)))

symintr >>> *** &&&
infixl (+) >>>
infixl (+) ***
infixl (+) &&&
overload >>> with co_arr_bind
overload *** with co_arr_combine
overload &&& with co_arr_fanout

implement main0(argc, argv) = let
  extern castfn __leak {v:view} (pf: v):<> void

  fun f1(x: int):<cloptr1> float = let val x' = g0i2f x in x' * 3.14159f end
  fun f2(x: int):<cloptr1> float = let val x' = g0i2f x in x' / 3.0f end

  fun event_vt_f1(x: event_vt int):<cloptr1> event_vt float = event_vt_map(x, f1)
  fun event_vt_f2(x: event_vt int):<cloptr1> event_vt float = event_vt_map(x, f2)

  #define U unit
  macdef aif(f) = co_arr<int,float>(,(f))
  macdef aeif(f) = co_arr<event_vt int,event_vt float>(,(f))

  val co = ints_from 2
  val co1 = aif(f1) &&& aif(f2)
  val co2 = co >>> co1
  val l = co_run_seq( co2, $list_vt{U}(U,U,U,U,U,U,U,U,U,U))
  val () = print_free_list2<float,float>(l)

  val co' = ints_evt_from 2
  val co1' = aeif(event_vt_f1) &&& aeif(event_vt_f2)
  val co2' = co' >>> co1'
  val l = co_run_seq(co2', $list_vt{U}(U,U,U,U,U,U,U,U,U,U))
  val () = print_free_list2<event_vt float,event_vt float>(l)

  val () = cloptr_free($UN.castvwtp0{cloptr0}(f1))
  val () = cloptr_free($UN.castvwtp0{cloptr0}(f2))
  val () = cloptr_free($UN.castvwtp0{cloptr0}(event_vt_f1))
  val () = cloptr_free($UN.castvwtp0{cloptr0}(event_vt_f2))
in
end
