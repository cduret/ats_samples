staload UN = "prelude/SATS/unsafe.sats"
staload "prelude/SATS/list_vt.sats"
staload _(*anonymous*) = "prelude/DATS/list.dats"
staload _(*anonymous*) = "prelude/DATS/list_vt.dats"

absviewtype
//coroutine_vt (inp: t@ype-, out: t@ype+) = ptr
coroutine_vt (inp: vt0p, out: vt0p) = ptr

//absviewtype
//coroutine_vt_vt (inp: vt0p, out: vt0p) = ptr

absviewtype event_vt (a: t@ype+) = ptr

typedef cfun1
  (a:vt0p, b:vt0p) = (a) -<cloref1> b // !!!!!

typedef fun1
  (a:vt0p, b:vt0p) = (a) -> b // !!!!!

viewtypedef lcfun1
  (a:vt0p, b:vt0p) = (a) -<lin,cloptr1> b

extern castfn coroutine_of_linc {a, b: vt0p}
(cf: lcfun1 (a, @(b, coroutine_vt (a, b)))):<> coroutine_vt (a, b)

extern castfn linc_of_coroutine {a, b:vt0p}
(co: coroutine_vt (a, b)):<> lcfun1 (a, @(b, coroutine_vt (a, b)))

extern castfn event_of_list {a: t@ype}{n: nat} (l: list_vt(a, n)):<> event_vt a
extern castfn list_of_event {a: t@ype} (e: event_vt a):<> [n: nat] list_vt(a, n)

extern fun{a,b: vt0p} co_run(co: coroutine_vt(a, b), x: a): (b,coroutine_vt(a,b))
extern fun{a,b: vt0p} co_run_seq{n: nat} (co: coroutine_vt(a, b), seq: list_vt(a, n)): list_vt(b, n)

// functor
extern fun{a,b,c: t@ype} co_fmap(co: coroutine_vt(a,b), f: cfun1(b, c)): coroutine_vt(a,c) // !!!
// category
extern fun{a,b,c: t@ype} co_cat_compose(cof: coroutine_vt(b, c), cog: coroutine_vt(a, b)): coroutine_vt(a, c)
// arrow
extern fun{a,b: t@ype} co_arr(f: cfun1(a, b)): coroutine_vt(a, b) // !!!
extern fun{a,b,c: t@ype} co_arr_first(co: coroutine_vt(a, b)): coroutine_vt((a,c), (b,c))
extern fun{a,b,c: t@ype} co_arr_second(co: coroutine_vt(a, b)): coroutine_vt((c,a), (c,b))
extern fun{a,b,c: t@ype} co_arr_bind(cof: coroutine_vt(a, b), cog: coroutine_vt(b, c)): coroutine_vt(a, c)
extern fun{a,b,c,d: t@ype} co_arr_combine(cof: coroutine_vt(a, b), cog: coroutine_vt(c, d)): coroutine_vt((a, c), (b, d))
extern fun{a,b,c: t@ype} co_arr_fanout(cof: coroutine_vt(a, b), cog: coroutine_vt(a, c)): coroutine_vt(a, (b,c))
extern fun{a, b: t@ype} co_arr_loop(co: coroutine_vt((a,b), b), seed: b): coroutine_vt(a, b)
// frp
extern fun{a,b: t@ype} co_scan(f: (a, b) -> a, i: a): coroutine_vt(b, a)
extern fun{a: t@ype} co_with_previous(first: a): coroutine_vt(a, (a,a))
extern fun{a: t@ype} co_delay(x: a): coroutine_vt(a,a)
extern fun{a: t@ype} co_integrate(x: a): coroutine_vt(a, a)
extern fun{a, b, c: t@ype} co_zipwith(f: (a, b) -> c): coroutine_vt((a,b), c)
extern fun{a: t@ype} co_derivate(): coroutine_vt(a, a)

extern fun{a: t@ype} co_watch(f: a -> bool): coroutine_vt(a, event_vt(a))

// events
extern fun{a: t@ype} event_vt_no_events(e: event_vt(a)): (event_vt(a), bool)
extern fun{a: t@ype} event_vt_any_events(e: event_vt(a)): (event_vt(a), bool)
extern fun{a: t@ype} event_vt_last(e: event_vt(a)): [b: bool] (event_vt(a), option_vt(a, b))
extern fun{a: t@ype} event_vt_dup(x: event_vt a): (event_vt a, event_vt a)
extern fun{a: t@ype} event_vt_free(x: event_vt a): void
extern fun{a,b: t@ype} event_vt_map(e: event_vt a, f: a -> b): event_vt b
extern fun{a,b: t@ype} event_vt_map_clo(e: event_vt a, f: a -<cloref1> b): event_vt b
extern fun{a: t@ype} event_vt_merge(e: event_vt a, e': event_vt(a)): event_vt(a)
extern fun{a, b: t@ype} event_vt_fold(e: event_vt a, f: (b, a) -> b, seed: b): b
extern fun{a: t@ype} event_vt_filter(e: event_vt a, f: a -> bool): event_vt(a)
#define NO_EVENTS event_of_list(list_vt_nil())
extern fun{a: t@ype} event_vt_tag(x: a): event_vt(a)

// events arrows
extern fun{a,b: t@ype} co_arr_event_vt(f: cfun1(event_vt a, event_vt b)): coroutine_vt(event_vt(a), event_vt(b))
extern fun{a,b: t@ype} co_arr_event_vt_fun(f: fun1(event_vt a, event_vt b)): coroutine_vt(event_vt(a), event_vt(b))
extern fun{a,b: t@ype} co_arr_event_vt_ex(f: cfun1(a, event_vt b)): coroutine_vt(a, event_vt(b))
extern fun{a,b: t@ype} co_arr_event_vt_ex_fun(f: fun1(a, event_vt b)): coroutine_vt(a, event_vt(b))
extern fun{a,b,c: t@ype} co_arr_event_vt_unzip(f: fun1(event_vt a, (event_vt b, event_vt c))): coroutine_vt(event_vt a, (event_vt b, event_vt c))
extern fun{a,b,c: t@ype} co_arr_event_vt_zip(f: fun1((event_vt a, event_vt b), event_vt c)): coroutine_vt((event_vt a, event_vt b), event_vt c)
extern fun{a,b,c: t@ype} co_arr_event_vt_first(co: coroutine_vt(event_vt a, event_vt b)): coroutine_vt((event_vt a,event_vt c), (event_vt b,event_vt c))
extern fun{a,b,c: t@ype} co_arr_event_vt_second(co: coroutine_vt(event_vt a, event_vt b)): coroutine_vt((event_vt c,event_vt a), (event_vt c,event_vt b))

// binding events
extern fun{a,b,c: t@ype} co_arr_event_vt_bind(cof: coroutine_vt(event_vt a, event_vt b), cog: coroutine_vt(event_vt b, event_vt c)): coroutine_vt(event_vt a, event_vt c)
extern fun{a,b,c,d,e: t@ype} co_arr_event_vt_bind12(cof: coroutine_vt(event_vt a, (event_vt b, event_vt c)), 
                                                    cog: coroutine_vt((event_vt b, event_vt c), (event_vt d, event_vt e))): 
                                                      coroutine_vt(event_vt a, (event_vt d, event_vt e))
extern fun{a,b,c,d,e: t@ype} co_arr_event_vt_bind21(cof: coroutine_vt((event_vt a, event_vt b), (event_vt c, event_vt d)), 
                                                    cog: coroutine_vt((event_vt c, event_vt d), event_vt e)): 
                                                      coroutine_vt((event_vt a, event_vt b), event_vt e)
extern fun{a,b,c,d: t@ype} co_arr_event_vt_bind121(cof: coroutine_vt(event_vt a, (event_vt b, event_vt c)), 
                                                   cog: coroutine_vt((event_vt b, event_vt c), event_vt d)): 
                                                     coroutine_vt(event_vt a, event_vt d)
extern fun{a,b,c,d,e,f: t@ype} co_arr_event_vt_bind2(cof: coroutine_vt((event_vt a, event_vt b), (event_vt c, event_vt d)), 
                                                     cog: coroutine_vt((event_vt c, event_vt d), (event_vt e, event_vt f))): 
                                                       coroutine_vt((event_vt a, event_vt b), (event_vt e, event_vt f))

// binding from signal to event
extern fun{a,b,c: t@ype} co_arr_bind_event_vt(cof: coroutine_vt(a, b), cog: coroutine_vt(b, event_vt c)): coroutine_vt(a, event_vt c)
extern fun{a,b,c: t@ype} co_arr_bind_event_vt_(cof: coroutine_vt(a, event_vt b), cog: coroutine_vt(event_vt b, event_vt c)): coroutine_vt(a, event_vt c)
extern fun{a,b,c,d: t@ype} co_arr_bind_event_vt_112(cof: coroutine_vt(a, event_vt b), 
                                                    cog: coroutine_vt(event_vt b, (event_vt c, event_vt d))): 
                                                       coroutine_vt( a, (event_vt c, event_vt d))
extern fun{a,b,c,d,e: t@ype} co_arr_bind_event_vt_122(cof: coroutine_vt(a, (event_vt b, event_vt c)), 
                                                      cog: coroutine_vt((event_vt b, event_vt c), (event_vt d, event_vt e))): 
                                                       coroutine_vt( a, (event_vt d, event_vt e))
extern fun{a,b,c,d,e: t@ype} co_arr_bind_event_vt_21(cof: coroutine_vt((a, b), (event_vt c, event_vt d)), 
                                                     cog: coroutine_vt((event_vt c, event_vt d), event_vt e)): 
                                                      coroutine_vt((a, b), event_vt e)
extern fun{a,b,c,d: t@ype} co_arr_bind_event_vt_121(cof: coroutine_vt(a, (event_vt b, event_vt c)), 
                                                    cog: coroutine_vt((event_vt b, event_vt c), event_vt d)): 
                                                      coroutine_vt(a, event_vt d)
extern fun{a,b,c,d,e,f: t@ype} co_arr_bind_event_vt_2(cof: coroutine_vt((a, b), (event_vt c, event_vt d)), 
                                                      cog: coroutine_vt((event_vt c, event_vt d), (event_vt e, event_vt f))): 
                                                        coroutine_vt((a, b), (event_vt e, event_vt f))


extern fun{a,b,c,d: t@ype} co_arr_event_vt_combine(cof: coroutine_vt(event_vt a, event_vt b), cog: coroutine_vt(event_vt c, event_vt d)): coroutine_vt((event_vt a, event_vt c), (event_vt b, event_vt d))
extern fun{a,b,c: t@ype} co_arr_event_vt_fanout(cof: coroutine_vt(event_vt a, event_vt b), cog: coroutine_vt(event_vt a, event_vt c)): coroutine_vt(event_vt a, (event_vt b, event_vt c))
extern fun{a, b: t@ype} co_arr_event_vt_loop(co: coroutine_vt((event_vt a, event_vt b), event_vt b), seed: event_vt b): coroutine_vt(event_vt a, event_vt b)

// event FRP
extern fun{a,b: t@ype} co_event_vt_scan(f: (event_vt a, event_vt b) -> event_vt a, i: event_vt a): coroutine_vt(event_vt b, event_vt a)
extern fun{a: t@ype} co_event_vt_with_previous(first: event_vt a): coroutine_vt(event_vt a, (event_vt a, event_vt a))
extern fun{a: t@ype} co_event_vt_delay(x: event_vt a): coroutine_vt(event_vt a, event_vt a)

extern fun{a, b: t@ype} co_arr_ev_map(f: a -> b): coroutine_vt(event_vt a, event_vt b)

implement{a,b} co_run(co, x) = res where {
  val f = linc_of_coroutine{a, b}(co)
  val res = f x
  val () = cloptr_free (f)
}

implement{a,b} co_run_seq(co, seq) = case+ seq of
  | ~list_vt_nil () => let 
    extern castfn __leak {v:view} (pf: v):<> void
    val () = __leak(co) // !!!!!
  in
    list_vt_nil()
  end
  | ~list_vt_cons (x, xs) => let
    val (res, co') = co_run<a, b>(co, x)
  in
    list_vt_cons(res, co_run_seq(co', xs))
  end

implement{a,b,c} co_fmap(co, f) = coroutine_of_linc{a, c}(llam x => let
  val (x', co') = co_run<a, b>(co, x)
in
  (f(x'), co_fmap<a, b, c>(co', f))
end )

implement{a,b} co_arr(f) = coroutine_of_linc{a,b}(llam x => (f x, co_arr(f)))

implement{a,b,c} co_arr_first(co) = coroutine_of_linc{(a,c), (b,c)}(llam (p) => let
    val (i, z) = p
    val (o, co') = co_run(co, i)
  in
    ((o, z), co_arr_first co')
  end
)
implement{a,b,c} co_arr_second(co) = coroutine_of_linc{(c,a), (c,b)}(llam (p) => let
    val (z, i) = p
    val (o, co') = co_run(co, i)
  in
    ((z, o), co_arr_second co')
  end
)
implement{a,b,c} co_arr_bind(cof, cog) = coroutine_of_linc{a,c}(llam i => let
  val (x, cof') = co_run(cof, i)
  val (y, cog') = co_run(cog, x)
in
  (y, co_arr_bind<a,b,c>(cof', cog'))
end )
implement{a,b,c,d} co_arr_combine(cof, cog) = co_arr_bind( co_arr_first cof, co_arr_second cog )

implement{a,b,c} co_arr_fanout(cof, cog) = co_arr_bind(co_arr f, co_arr_combine(cof, cog) ) where {
  fun f (x: a):<cloref1> (a, a) = (x, x)
}

// loop
implement{a, b} co_arr_loop(co, seed) = coroutine_of_linc{a, b}( llam x => let
  val (y, co') = co_run<(a,b),b> (co, (x, seed))
in
  (y, co_arr_loop<a,b>(co', y))
end )

// frp
implement{a,b} co_scan(f, i) = coroutine_of_linc{b, a}( llam x => let
  val a' = f(i, x)
in
  (a', co_scan(f, a'))
end )
implement{a} co_with_previous(first) = coroutine_of_linc{a, (a,a)}(llam i => ((i,first), step i)) where {
  fun step(old: a): coroutine_vt(a, (a, a)) = coroutine_of_linc{a, (a,a)}(llam i => ((i, old), step(i)))
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
implement{a} co_watch(f) = coroutine_of_linc{a, event_vt(a)}( llam x => if( f x ) then 
  (event_vt_tag<a>(x), co_watch f)
else 
  (NO_EVENTS, co_watch f) )

// events
implement{a} event_vt_tag(x) = event_of_list(list_vt_cons(x, list_vt_nil()))
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
implement{a} event_vt_last(e) = (e', last l') where {
  fun {a: t@ype} last {n:nat} (e: list_vt(a, n)): [b: bool] option_vt(a, b) = case+ e of
    | ~list_vt_cons (x, ~list_vt_nil()) => option_vt_some x
    | ~list_vt_cons (x, y) => last y 
    | ~list_vt_nil () => option_vt_none ()
  val l = list_of_event e
  val l' = list_vt_copy l
  val e' = event_of_list l
}
implement{a,b} event_vt_map(e,f) = let 
  val l = list_of_event e
  fun {a, b: t@ype} map_aux{n: nat} (l: list_vt(a, n), f: a -> b): list_vt(b, n) = case+ l of
    | ~list_vt_nil () => list_vt_nil()
    | ~list_vt_cons (x, xs) => list_vt_cons(f x, map_aux(xs, f))
in
  event_of_list{b}(map_aux(l, f))
end
implement{a,b} event_vt_map_clo(e,f) = let 
  val l = list_of_event e
  fun {a, b: t@ype} map_aux{n: nat} (l: list_vt(a, n), f: a -<cloref1> b): list_vt(b, n) = case+ l of
    | ~list_vt_nil () => list_vt_nil()
    | ~list_vt_cons (x, xs) => list_vt_cons(f x, map_aux(xs, f))
in
  event_of_list{b}(map_aux(l, f))
end
implement{a} event_vt_merge(e, e') = let
  val l = list_of_event e
  val l' = list_of_event e'
in
  event_of_list{a}(list_vt_append(l, l'))
end
implement{a,b} event_vt_fold(e, f, seed) = let
  val l = list_of_event e
  fun {a, b: t@ype} fold_aux{n: nat} (l: list_vt(a, n), f: (b,a) -> b, seed: b): b = case+ l of
    | ~list_vt_nil () => seed
    | ~list_vt_cons (x, xs) => f(fold_aux (xs, f, seed), x)
in
  fold_aux(l, f, seed)
end
implement{a} event_vt_filter(e, f) = let
  val l = list_of_event e
  fun {a:t@ype} filter_aux{n: nat} (l: list_vt(a, n), f: a -> bool): [m: nat | m <= n] list_vt(a, m) = case+ l of
    | ~list_vt_nil () => list_vt_nil()
    | ~list_vt_cons (x, xs) => if f(x) then list_vt_cons(x, filter_aux(xs, f)) else
      filter_aux(xs, f)
in
  event_of_list{a}(filter_aux(l, f))
end
implement{a} event_vt_dup(x) = let
  val l = list_of_event x
  val l' = list_vt_copy l
in (event_of_list l, event_of_list l') end
implement{a} event_vt_free(x) = let
  val l = list_of_event x
in
  list_vt_free l
end

// events arrows
implement{a,b} co_arr_event_vt(f) = coroutine_of_linc{event_vt a,event_vt b}(llam x => (f x, co_arr_event_vt(f)))
implement{a,b} co_arr_event_vt_fun(f) = coroutine_of_linc{event_vt a,event_vt b}(llam x => (f x, co_arr_event_vt_fun(f)))
implement{a,b} co_arr_event_vt_ex(f) = coroutine_of_linc{a,event_vt b}(llam x => (f x, co_arr_event_vt_ex(f)))
implement{a,b} co_arr_event_vt_ex_fun(f) = coroutine_of_linc{a,event_vt b}(llam x => (f x, co_arr_event_vt_ex_fun(f)))
implement{a,b,c} co_arr_event_vt_unzip(f) = coroutine_of_linc{event_vt a, (event_vt b, event_vt c)}(llam x => (f x, co_arr_event_vt_unzip f))
implement{a,b,c} co_arr_event_vt_zip(f) = coroutine_of_linc{(event_vt a, event_vt b), event_vt c}(llam x => (f x, co_arr_event_vt_zip f))
implement{a,b,c} co_arr_event_vt_first(co) = coroutine_of_linc{(event_vt a,event_vt c), (event_vt b,event_vt c)}(llam (p) => let
    val (i, z) = p
    val (o, co') = co_run(co, i)
  in
    ((o, z), co_arr_event_vt_first co')
  end
)
implement{a,b,c} co_arr_event_vt_second(co) = coroutine_of_linc{(event_vt c,event_vt a), (event_vt c,event_vt b)}(llam (p) => let
    val (z, i) = p
    val (o, co') = co_run(co, i)
  in
    ((z, o), co_arr_event_vt_second co')
  end )
implement{a,b,c} co_arr_event_vt_bind(cof, cog) = coroutine_of_linc{event_vt a, event_vt c}(llam i => let
      val (x, cof') = co_run(cof, i)
      val (y, cog') = co_run(cog, x)
    in
      (y, co_arr_event_vt_bind<a,b,c>(cof', cog'))
    end )
implement{a,b,c} co_arr_bind_event_vt(cof, cog) = coroutine_of_linc{a, event_vt c}(llam i => let
      val (x, cof') = co_run(cof, i)
      val (y, cog') = co_run(cog, x)
    in
      (y, co_arr_bind_event_vt<a,b,c>(cof', cog'))
    end )
implement{a,b,c} co_arr_bind_event_vt_(cof, cog) = coroutine_of_linc{a, event_vt c}(llam i => let
      val (x, cof') = co_run(cof, i)
      val (y, cog') = co_run(cog, x)
    in
      (y, co_arr_bind_event_vt_<a,b,c>(cof', cog'))
    end )
implement{a,b,c,d} co_arr_bind_event_vt_112(cof, cog) = coroutine_of_linc{a, (event_vt c, event_vt d)}(llam i => let
      val (x, cof') = co_run(cof, i)
      val (y, cog') = co_run(cog, x)
    in
      (y, co_arr_bind_event_vt_112<a,b,c,d>(cof', cog'))
    end )
implement{a,b,c,d,e} co_arr_bind_event_vt_122(cof, cog) = coroutine_of_linc{a, (event_vt d, event_vt e)}(llam i => let
      val (x, cof') = co_run(cof, i)
      val (y, cog') = co_run(cog, x)
    in
      (y, co_arr_bind_event_vt_122<a,b,c,d,e>(cof', cog'))
    end )
implement{a,b,c,d,e} co_arr_bind_event_vt_21(cof, cog) = coroutine_of_linc{(a, b), event_vt e}(llam i => let
  val (x, cof') = co_run(cof, i)
  val (y, cog') = co_run(cog, x)
in
  (y, co_arr_bind_event_vt_21<a,b,c,d,e>(cof', cog'))
end )
implement{a,b,c,d} co_arr_bind_event_vt_121(cof, cog) = coroutine_of_linc{a, event_vt d}(llam i => let
  val (x, cof') = co_run(cof, i)
  val (y, cog') = co_run(cog, x)
in
  (y, co_arr_bind_event_vt_121<a,b,c,d>(cof', cog'))
end )
implement{a,b,c,d,e,f} co_arr_bind_event_vt_2(cof, cog) = coroutine_of_linc{(a, b), (event_vt e, event_vt f)}(llam i => let
    val (x, cof') = co_run(cof, i)
    val (y, cog') = co_run(cog, x)
  in
    (y, co_arr_bind_event_vt_2<a,b,c,d,e,f>(cof', cog'))
  end )
implement{a,b,c,d,e,f} co_arr_event_vt_bind2(cof, cog) = coroutine_of_linc{(event_vt a, event_vt b), (event_vt e, event_vt f)}(llam i => let
    val (x, cof') = co_run(cof, i)
    val (y, cog') = co_run(cog, x)
  in
    (y, co_arr_event_vt_bind2<a,b,c,d,e,f>(cof', cog'))
  end )
implement{a,b,c,d,e} co_arr_event_vt_bind12(cof, cog) = coroutine_of_linc{event_vt a, (event_vt d, event_vt e)}(llam i => let
  val (x, cof') = co_run(cof, i)
  val (y, cog') = co_run(cog, x)
in
  (y, co_arr_event_vt_bind12<a,b,c,d,e>(cof', cog'))
end )
implement{a,b,c,d,e} co_arr_event_vt_bind21(cof, cog) = coroutine_of_linc{(event_vt a, event_vt b), event_vt e}(llam i => let
  val (x, cof') = co_run(cof, i)
  val (y, cog') = co_run(cog, x)
in
  (y, co_arr_event_vt_bind21<a,b,c,d,e>(cof', cog'))
end )
implement{a,b,c,d} co_arr_event_vt_bind121(cof, cog) = coroutine_of_linc{event_vt a, event_vt d}(llam i => let
  val (x, cof') = co_run(cof, i)
  val (y, cog') = co_run(cog, x)
in
  (y, co_arr_event_vt_bind121<a,b,c,d>(cof', cog'))
end )
implement{a,b,c,d} co_arr_event_vt_combine(cof, cog) = co_arr_event_vt_bind2( co_arr_event_vt_first cof, co_arr_event_vt_second cog )
implement{a,b,c} co_arr_event_vt_fanout(cof, cog) = let
  val co_dup = co_arr_event_vt_unzip<a,a,a>(event_vt_dup)
  val co = co_arr_event_vt_combine<a,b,a,c>(cof, cog)
in
  co_arr_event_vt_bind12(co_dup, co)
end

// events loop
implement{a, b} co_arr_event_vt_loop(co, seed) = coroutine_of_linc{event_vt a, event_vt b}( llam x => let
  val (y, co') = co_run(co, (x, seed))
  val (y', y'') = event_vt_dup y
in
  (y', co_arr_event_vt_loop<a,b>(co', y''))
end )

// events frp
implement{a,b} co_event_vt_scan(f, i) = coroutine_of_linc{event_vt b, event_vt a}( llam x => let
  val y = f(i, x)
  val (y', y'') = event_vt_dup y
in
  (y', co_event_vt_scan(f, y''))
end )
implement{a} co_event_vt_with_previous(first) = coroutine_of_linc{event_vt a, (event_vt a, event_vt a)}(llam i => let
  val (i', i'') = event_vt_dup i
  fun step(old: event_vt a): coroutine_vt(event_vt a, (event_vt a, event_vt a)) = coroutine_of_linc{event_vt a, (event_vt a, event_vt a)}(llam i => let
    val (i', i'') = event_vt_dup i
  in ((i', old), step i'') end)
in
  ((i',first), step i'')
end)
// unzip
implement{a} co_event_vt_delay(x) = co_arr_event_vt_bind121(co_event_vt_with_previous x, co_arr_event_vt_zip(f)) where {
  fun f(p: (event_vt a, event_vt a)): event_vt a = let 
    val (fst, snd) = p
    val () = event_vt_free fst
  in snd end
}

implement{a,b} co_arr_ev_map(f) = co_arr_event_vt<a, b>(lam e => event_vt_map(e, f))

extern fun ints_from (x: int): coroutine_vt (unit, int)
implement ints_from(x) = coroutine_of_linc{unit,int}(llam _ => (x, ints_from (x+1)))

//extern fun ints_evt_from (x: int): coroutine_vt (unit, event_vt int)
//implement ints_evt_from(x) =
  //co_arr_bind_event_vt(ints_from x, co_arr_event_vt_ex_fun<int, int>(event_vt_tag))

implement main(argc, argv) = let
  extern castfn __leak {v:view} (pf: v):<> void

  fun free_lst{n: nat} (l: list_vt(int, n)): void = case+ l of
    | ~list_vt_nil () => ()
    | ~list_vt_cons (_, xs) => free_lst(xs)

  fun print_res{n: nat} (l: list_vt(int, n)): void = case+ l of
  | ~list_vt_nil () => ()
  | ~list_vt_cons (x, xs) => let
      val () = printf("%d ", @(x))
    in
      print_res xs
    end
  fun print_resf{n: nat} (l: list_vt(float, n)): void = case+ l of
  | ~list_vt_nil () => ()
  | ~list_vt_cons (x, xs) => let
      val () = printf("%f ", @(double_of_float x))
    in
      print_resf xs
    end
  fun print_res2{n: nat} (l: list_vt((float,float), n)): void = case+ l of
  | ~list_vt_nil () => ()
  | ~list_vt_cons (x, xs) => let
      val () = printf("(%f, %f) ", @(double_of_float x.0, double_of_float x.1))
    in
      print_res2 xs
    end
  fun print_res2_{n: nat} (l: list_vt((event_vt float, event_vt float), n)): void = case+ l of
  | ~list_vt_nil () => ()
  | ~list_vt_cons (x, xs) => let
      val (e,evts) = event_vt_any_events x.0
      val (e',evts') = event_vt_any_events x.1
      val () = if evts || evts' then () where {
        val () = print("( ")
        val () = print_resf(list_of_event{float} e)
        val () = print_resf(list_of_event{float} e')
        val () = print(")\n")
      } else () where {
        val () = event_vt_free<float>(e)
        val () = event_vt_free<float>(e')
      }
    in
      print_res2_ xs
    end

  val co = ints_from 2
  val co1 = co_arr_bind<unit, int, (int,int)>(co, co_arr<int, (int,int)>(lam (x) => (x,x)))
  val co2 = co_arr<int, float>(lam (x) => let val x' = float_of_int x in x' * 3.14159f end)
  val co3 = co_arr<int, float>(lam (x) => let val x' = float_of_int x in x' / 3.0f end)
  val co4 = co_arr_combine<int, float, int, float>(co2, co3) // (int,int) -> (float,float)
  val co5 = co_arr_bind<unit, (int,int), (float, float)>(co1, co4)
  val l = co_run_seq( co5, list_vt_cons(unit, list_vt_cons(unit, list_vt_cons(unit, list_vt_nil() ))) )
  val () = print_res2 l val () = print "\n"

// events test

  fun pred(x: int): bool = x > 1 && x < 4
  val co' = co_arr_bind_event_vt<unit,int,int>(ints_from 0, co_watch<int>(pred)) // unit -> event_vt int
  val co1' = co_arr_bind_event_vt_112<unit,int,int,int>(co', co_arr_event_vt_unzip<int,int,int>(event_vt_dup)) // unit -> (event_vt int, event_vt int)
  val co2' = co_arr_event_vt<int, float>(lam (e) => event_vt_map<int,float>(e, lam (x) => let val x' = float_of_int x in x' * 3.14159f end)) // event_vt int -> event_vt float
  val co3' = co_arr_event_vt<int, float>(lam (e) => event_vt_map<int,float>(e, lam (x) => let val x' = float_of_int x in x' / 3.0f end)) // event_vt int -> event_vt float
  val co4' = co_arr_event_vt_combine<int, float, int, float>(co2', co3') // (event_vt int, event_vt int) -> (event_vt float, event_vt float)
  val co5' = co_arr_bind_event_vt_122<unit,int,int,float, float>(co1', co4') // unit -> (event_vt float, event_vt float)
  val l = co_run_seq( co5', list_vt_cons(unit, list_vt_cons(unit, list_vt_cons(unit, list_vt_cons(unit, list_vt_nil() )))) )
  val () = print_res2_ l

in
end
