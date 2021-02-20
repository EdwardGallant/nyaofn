" in case we resource this file, don't actually reset NyaoFn as we now have
" dependencies like nyaogit that could add to it
if !exists('g:NyaoFn')
  let g:NyaoFn = {}
endif
let s:N = g:NyaoFn

" command wrappers {{{

fu! s:_Echo(msg)
  echo a:msg
  return a:msg
endfu

fu! s:_Echom(msg)
  echom a:msg
  return a:msg
endfu

fu! s:_Echoerr(msg)
  echoerr a:msg
  return a:msg
endfu

fu! s:_LetObj(obj, key, value)
  let a:obj[a:key] = a:value
  return a:value
endfu

fu! s:_Unlet(var)
  silent exe "normal! :unlet ".a:var."\<CR>"
  return a:var
endfu

fu! s:_Cd(path)
  exe "normal! :cd ".a:path."\<CR>"
  return a:path
endfu

fu! s:_Edit(path)
  exe "normal! :edit ".a:path."\<CR>"
  return a:path
endfu

fu! s:_SplitEdit(path)
  exe "normal! :sp ".a:path."\<CR>"
  return a:path
endfu

" fu! s:_SilentSplitEdit(path)
"   exe "normal! :silent sp ".a:path."\<CR>"
"   return a:path
" endfu

fu! s:_Explore(path)
  exe "normal! :Explore ".a:path."\<CR>"
  return a:path
endfu

let s:N.Sequence = { fns -> map(fns, {_, fn -> fn()}) }
let s:N.LazySequence = { fns -> { -> map(fns, {_, fn -> fn()}) }}

" This can be used like Trace... Maybe should rename it, but oh well
" I'll just alias it
" s:N.Echo :: msg -> () msg
let s:N.Echo      = { msg -> s:_Echo(msg) }
let s:N.Echom     = { msg -> s:_Echom(msg) }
let s:N.Echoerr   = { msg -> s:_Echoerr(msg) }
let s:N.Unlet     = { var -> s:_Unlet(var) }
let s:N.Cd        = { path -> s:_Cd(path) }
let s:N.Edit      = { path -> s:_Edit(path) }
let s:N.SplitEdit = { path -> s:_SplitEdit(path) }
" let s:N.SilentSplitEdit = { path -> s:_SilentSplitEdit(path) }
let s:N.Explore   = { path -> s:_Explore(path) }
let s:N.LazyEcho  = { msg -> { -> s:_Echo(msg) } }
let s:N.Trace     = { msg -> s:_Echo(msg) }
let s:N.Input     = { str -> input(str) }

" call s:N.Sequence([s:N.LazyEcho('A'), s:N.LazyEcho('B') ])

" LetObj :: obj -> key -> value
let s:N.LetObj = { obj -> { key -> { value -> s:_LetObj( obj, key, value ) } } }
" let some_obj = {}
" call s:N.LetObj( some_obj )( 'test' )( 'value' )
" echo some_obj
" }}}
" Monads {{{
" This will probably be a little annoying to implement in vimscript, so lets
" save it for never
" let s:N.Just = {
" of: { a ->  }
"       \ }


" Can be useful at least in terms of having a false value that isn't 0
let s:N.Nothing = { 'name': 'Nothing' }
" Nothing does not actually need to implement Map, it is just the value inside
" the Maybe
      " \ 'map': { a -> s:N.Nothing }

let s:N.IsNothing    = { a -> type(a) == v:t_dict && has_key(a, 'name') && a.name == 'Nothing' }
let s:N.IsNotNothing = { a -> !s:N.IsNothing(a) }
let s:N.Falsey       = { v -> v == '' || v == 0 || v == [] || s:N.IsNothing(v) }
let s:N.Truthy       = { v -> !s:N.Falsey(v) }

fu! s:_Of(v) dict
  let o = copy(self)
  let o.v = a:v
  return o
endfu

" I think rather than falsey, I explicitly only want to pass through if it is
" specifically a Nothing
fu! s:_MaybeMap(f) dict
  return s:N.IsNothing( self.v ) ? self : s:N.Maybe.Of( a:f( self.v ) )
endfu

fu! s:_MaybeIsNothing() dict
  return s:N.IsNothing(self.v)
endfu

let s:N.Maybe = { 'Of': function('s:_Of')
              \ , 'Map': function('s:_MaybeMap')
              \ , 'IsNothing': function('s:_MaybeIsNothing')
              \ }

let s:N.maybe   = { x -> s:N.Maybe.Of(x) }

let s:N.maybeMap = { f -> { maybe -> s:N.IsNothing( maybe.v ) ? maybe : s:N.Maybe.Of( f( maybe.v ) ) } }

" this would only work if the function only expected one more arg... likely to
" confuse me
" let s:N.MaybeFn = { f -> { x -> s:N.Maybe.of(f(x)) } }

" echo s:N.Maybe.of(3) != s:N.Maybe.of(5)
" echo s:N.Maybe.Of(3).Map({ x -> x*3 }).Map({ a -> a*10 })

  " \ , isNothing: { -> s:N.Falsey(self.v) }
  " \ , isJust:
  " \ , ap:
  " \ , chain:
  " \ , join:
  " \ , sequence:
  " \ , traverse:
  " \ , map: { fn ->  }
  " \ }

let s:N.Id = { x -> x }

" echo s:N.Falsey( '' )
" echo s:N.Truthy( '' )

" }}}
" array {{{
" for testing various things... helpful that it's
" not communicative (associative?)
let s:N.Concat = { a -> { b -> a.b } }
let s:N.ConcatA = s:N.Concat('A')
let s:N.ConcatB = s:N.Concat('B')
let s:N.ConcatC = s:N.Concat('C')

let s:N.ConcatArray = { a -> { b -> a+b } }

" may get confusing since generic functional example always makes an add
" integers function... could rename this to be less bad. Push... but also
" it's not functional, since it modifies state... which we kind of want, kind
" of don't want... oh no... will work on this later
" TODO:
" let s:N.Add = { item -> { ary -> add(ary, item) } }

let s:N.Flip = { fn -> { a -> { b -> fn(b)(a) } } }

let s:N.Fst = { ary -> ary[0] }
let s:N.Snd = { ary -> ary[1] }

let s:N.First = { n -> { ary -> ary[0:n-1] } }
let s:N.Last = { n -> { ary -> ary[-n:-1] } }

let s:N.Eq = { a -> { b -> a == b } }
let s:N.NotEq = { a -> { b -> a != b } }

fu! s:_DropWhile( predicate, ary )
  let r = copy(a:ary)
  for i in r
    if a:predicate( i ) | call remove(r, 0) | else | return r | endif
  endfor
endfu

fu! s:_TakeWhile( predicate, ary )
  let r = []
  for i in a:ary
    if a:predicate( i ) | call add(r, i) | else | return r | endif
  endfor
endfu

fu! s:_DropTakeWhile( predicate, ary )
  let take_r = []
  let drop_r = copy(a:ary)
  for i in a:ary
    if a:predicate( i ) | call add(take_r, i) | call remove(drop_r, 0) | else | return [take_r, drop_r] | endif
  endfor
endfu

fu! s:_SplitAryAt( predicate, ary )
  let r = []
  let g = []
  for i in a:ary
    if a:predicate(i)
      if len(g) > 0
        call add(r,g)
        let g = []
      endif
    else
      call add(g, i)
    endif
  endfor
  if len(g) > 0 | call add(r,g) | endif
  return r
endfu

fu! s:_SplitAryAtKeepSep( predicate, ary )
  let r = []
  let g = []
  for i in a:ary
    if a:predicate(i)
      if len(g) > 0
        call add(g,i)
        call add(r,g)
        let g = []
      endif
    else
      call add(g,i)
    endif
  endfor
  if len(g) > 0 | call add(r,g) | endif
  return r
endfu

let s:N.TakeWhile = { pred -> { ary -> s:_TakeWhile( pred, ary ) } }
let s:N.DropWhile = { pred -> { ary -> s:_DropWhile( pred, ary ) } }
let s:N.DropTakeWhile = { pred -> { ary -> s:_DropTakeWhile( pred, ary ) } }

let s:N.SplitAryAt        = { pred -> { ary -> s:_SplitAryAt( pred, ary ) } }
let s:N.SplitAryAtKeepSep = { pred -> { ary -> s:_SplitAryAtKeepSep( pred, ary ) } }

" I have a new concept where I would want to split the array at the same
" place, but keep the splitting part as the first element.. not sure what that
" should be called

" s:N.Paragraphs :: [String] -> [[String]]
let s:N.Paragraphs = s:N.SplitAryAt( s:N.Eq('') )

" this looks kinda bad, also it's recursive and we don't want to do that in
" vimscript.. think I found this one on the internet, and I've copied it here
" from my misc/lib
fu! s:_Flatten(list)
    let val = []
    for elem in a:list
        if type(elem) == type([])
            call extend(val, s:_Flatten(elem))
        else
            call add(val, elem)
        endif
        unlet elem
    endfor
    return val
endfu

let s:N.Flatten = { ary -> s:_Flatten(ary) }

" as far as I can tell, copy() is not necessary here , [:] slice will make a
" new array
let s:N.Take = { int -> { ary -> ary[0:int-1] } }
let s:N.Drop = { int -> { ary -> ary[int:-1] } }

" }}}

let s:N.Prop = { prop -> { dict -> dict[prop] } }
let s:N.SetProp = s:N.LetObj

" function functions {{{1
let s:N.C             = { f -> { g -> { x -> f(g(x))}}}
let s:N.Compose       = s:N.C
let s:N.CV            = { f -> { g -> { -> f(g())}}}
let s:N.ComposeToVoid = s:N.CV
let s:N.LC            = { f -> { g -> { x -> { -> f(g(x)) }}}}
let s:N.LazyCompose   = s:N.LC

" possibly useful if you need to do some work with a value in compose, but
" then want to pass the same value along and discard the result of the last
" function... eg side effects, so very dirty and non functional, but maybe
" convenient for hacky little things
let s:N.Tap = { f -> { x -> [f(x), x][1] } }
" echo s:N.Tap( s:N.Take(1) )( [1,2,3] )

let s:N.DiscardFn     = { f -> { _ -> f() } }
let s:N.DF            = s:N.DiscardFn
let s:N.LazyDiscardFn = { f -> { x -> { _ -> f(x) } } }
let s:N.LDF           = s:N.LazyDiscardFn

" echo s:N.DiscardFn( { -> 'bears' } )( 20 )

" let s:N.ThisLine = s:N.LazyGetLine('.')
" call s:N.CV( s:N.Echo )( s:N.ThisLine )()
" call s:N.CV( s:N.Echo )( s:N.ThisLine )()

" let s:N.GetLine = { expr -> getline(expr) }
" call s:N.LC( s:N.Echo )( s:N.GetLine )('.')()
" call s:N.LC( s:N.Echo )( s:N.GetLine )('.')()

" }}}1
" more array {{{1
" kind of sad that we have to use slow version of a function
" maybe in vim 9 the new function type will let us add to this more easily
fu! s:_Reduce( fn, initial, ary )
  " must be capital R, because it could be a funcref and vimscript hates you
  let R = a:initial
  for A in a:ary
    let R = a:fn(R)(A)
  endfor
  return R
endfu
let s:N.Reduce = { fn -> { initial -> { ary -> s:_Reduce(fn, initial, ary) } } }
let s:N.ReduceR = { fn -> { initial -> { ary -> s:_Reduce(fn, initial, reverse(ary)) } } }

fu! s:_Fold( fn, ary )
  let R = a:ary[0]
  for A in a:ary[1:-1]
    let R = a:fn(R)(A)
  endfor
  return R
endfu
let s:N.Fold = { fn -> { ary -> s:_Fold(fn, ary) } }
let s:N.FoldR = { fn -> { ary -> s:_Fold(fn, reverse(ary)) } }
" these changes might be breaking, didn't realise at all that map was
" modifying the array
let s:N.Map = { fn -> { ary -> map(copy(ary), {_, v -> fn(v)}) } }
let s:N.Filter = { fn -> { ary -> filter(copy(ary), {_, v -> fn(v)}) } }
let s:N.Reject = { fn -> { ary -> filter(copy(ary), {_, v -> !fn(v)}) } }

" }}}1
" evil helpers {{{
fu! s:_Call( fn, ary )
  let FN = a:fn
  for A in a:ary | let FN = FN(A) | endfor
  return FN
endfu

let s:N.Call     = { fn -> { ary -> s:_Call(fn, ary) } }
let s:N.LazyCall = { fn -> { ary -> { -> s:_Call(fn, ary) } } }

" let s:Add = { a -> { b -> { c -> a . b . c } } }

" echo s:N.Call( s:Add )(['a'
"                      \ ,'b'
"                      \ ,'c'])


" }}}

" funcref by sort() is not normally curried, but I still want to be able to
" pass a curried function, so we wrap the first arg in a lamda that takes two
" arguments, odd as it looks
" Sort :: (a -> a -> 1 | 0 | -1 ) -> [a] -> [a]
let s:N.Sort = { f -> { ary -> sort(copy(ary), { a, b -> f(a)(b) }) } }

" if you wanted distant items to be grouped, you would sort first to remove
" that distance... This group by is based (from the briefest of glances) on
" the type signature and behaviour of Haskell's group by
" GroupBy :: (a -> a -> bool) -> [a] -> [[a]]
let s:N.GroupBy = { f -> { ary ->
  \ s:N.Reduce( { aoa -> { item ->
    \ len(aoa) == 0
    \ ? add(aoa, [item])
    \ : f(aoa[-1][-1])(item)
      \ ? s:N.Snd([ add(aoa[-1], item), aoa ])
      \ : add(aoa, [item]) } } )( [] )( ary )
\ } }

fu! s:_GroupToObjHelper( fn, obj, val )
  let k = a:fn( a:val )
  if !has_key(a:obj, k)
    let a:obj[k] = []
  endif
  call add(a:obj[k], a:val)
  return a:obj
endfu
let s:GroupToObjHelper = { fn -> { obj -> { val -> s:_GroupToObjHelper(fn, obj, val) } } }

let s:N.GroupToObj = { f -> { ary -> s:N.Reduce( s:GroupToObjHelper(f) )({})(ary) } }

" Should actually return Maybe, if something is found then the value,
" otherwise Nothing
fu! s:_Find( predicate, ary )
  for Item in a:ary
    if a:predicate(Item) | return Item | endif
  endfor
  return s:N.Nothing
endfu
let s:N.Find = { predicate -> { ary -> s:_Find( predicate, ary) } }
let s:N.MaybeFind = { predicate -> { ary -> s:N.maybe(s:_Find( predicate, ary)) } }

" wonder if there is a nicer way of doing this
let s:N.Contains = { predicate -> { ary -> s:N.IsNotNothing( s:N.Find(predicate)(ary) ) } }

" this should be rewritten in a more functional way, but I'm too tired
fu! s:_Lens( keys, obj )
  let o = a:obj
  for k in a:keys
    if has_key(o, k)
      let o = o[k]
    else
      return s:N.Nothing
    endif
  endfor
  return o
endfu

let s:N.Lens = { keys -> { obj -> s:_Lens(keys, obj) } }
let s:N.MaybeLens = { keys -> { obj -> s:N.maybe(s:_Lens(keys, obj)) } }


" Might not work... composing with the g being a two argument function
" let s:N.Contains       = s:N.C( s:N.IsNotNothing )( s:N.Find )
" let s:N.DoesNotContain = s:N.C( s:N.IsNothing )( s:N.Find )

" echo s:N.Find( { a -> a == 'A' })( [1,2, 'A'] )

" not sure yet what this should be... basically it is supposed to make every
" item first, then join them by folding. But the items are expected to be
" monoids that know how to fold themselves, in my case I probably have to
" specify one function to fold with, and another to map with
" let s:N.ReduceRMap =

fu! s:_Exe( cmd, arg )
  exe ':'.a:cmd.' '.a:arg
endfu

fu! s:_Normal( cmd )
  exe "normal " . a:cmd
endfu

fu! s:_NormalB( cmd )
  exe "normal! " . a:cmd
endfu

" call s:_NormalB("\<C-w>o")

let s:N.System     = { process -> system(process) }
let s:N.LazySystem = { process -> { -> system(process) } }

let s:N.SystemLines     = { process -> systemlist(process) }
let s:N.LazySystemLines = { process -> { -> systemlist(process) } }

" SetQFList :: [{}] -> ?
let s:N.SetQFList = { ary -> setqflist( ary ) }

" note the last arg is the key you want to trigger, eg because you could want
" to map more than one key to the same thing, but the other way around is not
" possible (except maybe for buflocal mapping or something like that)
fu! s:_Map( mode, flags, rhs, lhs )
  exe ''.a:mode.'no '.a:flags->split(',')->map({_, v -> '<'.v.'>'})->join('').' '.a:lhs.' '.a:rhs
  return ''.a:mode.'no '.a:flags->split(',')->map({_, v -> '<'.v.'>'})->join('').' '.a:lhs.' '.a:rhs
endfu
let s:N.Remap = { mode -> { flags -> { rhs -> { lhs -> s:_Map(mode, flags, rhs, lhs)}}}}
let s:N.Nno = s:N.Remap('n')
let s:N.Vno = s:N.Remap('v')
let s:N.Ino = s:N.Remap('i')

let s:N.BufNno = s:N.Nno('buffer')
let s:N.BufVno = s:N.Vno('buffer')
" call s:N.Nno('silent')(':call EnterNormalMode()<CR>')('w')
" call s:N.Nno('')(':call EnterNormalMode()<CR>')('w')

" let s:N.AddABC = s:N.ReduceR(  )

" echo s:N.Map( s:N.ConcatA )( ['a','b','c'] )
" echo s:N.Reduce(s:N.Concat)( 'D' )(['A','B','C'])
" echo s:N.ReduceR(s:N.Concat)( 'D' )(['A','B','C'])

" eg Haskell $, just apply a function, but sadly not that helpful since we
" don't have associativity value (which would be 0, or do me last in Haskell)
" let s:N.Ap = { fn -> { a -> { b -> fn(a)(b) } } }

" echo s:N.Map( s:N.C )( [ s:N.ConcatA, s:N.ConcatB, s:N.ConcatC ] )
" echo s:N.ReduceR( s:N.Ap )( s:N.C(s:N.ConcatA) )( s:N.Map( s:N.C )( [ s:N.ConcatA, s:N.ConcatB, s:N.ConcatC ] ) )

" echo s:N.ConcatA( 'ksjdflk')

" So here is our way of composing functions with a little less pain
" echo s:N.ReduceR( s:N.C )( [ s:N.ConcatA, s:N.ConcatB, s:N.ConcatC ] )('test')

let s:N.Type     = { a -> type(a) }
let s:N.ToString = { a -> string(a) }

" This is a little counter intuitive, but regular foward reduce gives us the
" correct result, because compose already sort of makes things backward on its
" own, so you don't need to ReduceR... ReduceR is probably Pipe()
let s:N.FoldCompose = s:N.Fold( s:N.C )
let s:N.FC = s:N.FoldCompose
" call s:N.FoldCompose( [ s:N.ConcatA, s:N.Echo, s:N.ConcatB, s:N.Echo, s:N.ConcatC, s:N.Concat('D') ] )('test')

" fu! _FoldComposeToVoid(ary)
"   let but_one 
" endfu


let s:N.FoldComposeToVoid = { ary -> s:N.CV( s:N.FoldCompose( ary[0:-2] ) )( ary[-1]) }
let s:N.FCTV              = s:N.FoldComposeToVoid

let s:N.LazyFoldCompose   = { ary -> s:N.LC( s:N.FoldCompose( ary[0:-3] ) )( ary[-2])(ary[-1]) }
let s:N.LFC               = s:N.LazyFoldCompose

" let s:N.LazyGetLine = { expr -> { -> getline(expr) } }
" let s:N.ThisLine    = s:N.LazyGetLine('.')
" let s:N.GetLine = { expr -> getline(expr) }
" echo s:N.FoldComposeToVoid( [ s:N.ConcatA, s:N.Echo, s:N.ConcatB, s:N.Echo, s:N.ConcatC, s:N.ThisLine ] )()
" echo s:N.LazyFoldCompose( [ s:N.ConcatA, s:N.Echo, s:N.ConcatB, s:N.Echo, s:N.ConcatC, s:N.GetLine, '.' ] )()

let s:N.Exe                  = { cmd -> { arg -> s:_Exe( cmd, arg ) } }
let s:N.LazyExe              = { cmd -> { arg -> { -> s:_Exe( cmd, arg ) } } }
let s:N.Normal               = { cmd -> s:_Normal(cmd) }
let s:N.NormalB              = { cmd -> s:_NormalB(cmd) }
let s:N.LazyNormal           = { exp -> { -> s:N.Exe("normal! ")( exp ) } }
let s:N.Chrome               = { link -> s:N.Normal(":!open -a chrome ".shellescape(link)."\<CR>") }
let s:N.LazyChrome           = { link -> { -> s:N.Chrome(link) } }
let s:N.BufNr                = { expr -> bufnr(expr) }
let s:N.ThisBufNr            = { -> s:N.BufNr('%') }
let s:N.MatchStr             = { pattern -> { str -> matchstr(str, pattern) } }
let s:N.MatchStrBool         = { pattern -> { str -> matchstr(str, pattern) != '' } }
" echo s:N.MatchStrBool('\v/[^/]*\.[^/]*$')("lksjdlkslkj/test")
let s:N.MatchLink            = s:N.MatchStr( 'http[s]\?:\/\/[[:alnum:]%\/_#.-]*' )
let s:N.MatchFirstWord       = s:N.MatchStr( '\v^\w*' )
let s:N.LazyExpand           = { str -> { -> expand(str) } }
let s:N.WordUnderCursor      = s:N.LazyExpand('<cWORD>')
let s:N.MatchLinkUnderCursor = { -> s:N.MatchLink( s:N.WordUnderCursor() ) }
let s:N.GetLine              = { expr -> getline(expr) }
let s:N.LazyGetLine          = { expr -> { -> getline(expr) } }
let s:N.LazyGetLineNr        = { expr -> { -> line(expr) } }
let s:N.ThisLine             = s:N.LazyGetLine('.')
let s:N.ThisLineNr           = s:N.LazyGetLineNr('.')
let s:N.CurrentFilename      = s:N.LazyExpand('%:p')

let s:N.DeleteThisLine = { -> deletebufline(bufnr("%"), line('.')) }
let s:N.DeleteLines    = { start -> { end -> deletebufline(bufnr("%"), start, end) } }

fu! s:_GetVisualSection()
  let [line_start, column_start] = getpos("'<")[1:2]
  let [line_end, column_end] = getpos("'>")[1:2]
  let lines = getline(line_start, line_end)
  if len(lines) == 0
    return ''
  endif
  let lines[-1] = lines[-1][: column_end - (&selection == 'inclusive' ? 1 : 2)]
  let lines[0] = lines[0][column_start - 1:]
  return join(lines, "\n")
endfunction

let s:N.GetVisualSelection = {  -> s:_GetVisualSection() }

" this seems to do the job right... what gives
" vno <f3> :<C-u>call g:NyaoFn.Map(g:NyaoFn.Echo)(g:NyaoFn.Lines(g:NyaoFn.GetVisualSelection()))<CR>

let s:N.GetVisualStartEnd = {  -> [getpos("'<"), getpos("'>")] }
let s:N.GetVisualLineStartEnd = {  -> [getpos("'<")[1], getpos("'>")[1]] }

fu! s:_DeleteSelectedLines()
  let lines = s:N.GetVisualLineStartEnd()
  call s:N.DeleteLines( lines[0] )( lines[1] )
endfu

let s:N.DeleteSelectedLines = {  -> s:_DeleteSelectedLines() }

" vno <buffer> <f3> :<C-u>echo g:NyaoFn.GetVisualLineStartEnd()<CR>



let s:N.Substitute           = { flags -> { pattern -> { replacement -> { string -> substitute(string, pattern, replacement, flags) } } } }

" echo s:N.CurrentFilename()

" FindStrInAry :: str -> ary -> bool
" let s:N.FindStrInAry = { str -> s:N.Contains( s:N.MatchStrBool(str) ) }
let s:N.FindStrInAry = { str -> s:N.Contains( s:N.MatchStrBool(str) ) }

let s:N.ToUpper = { str -> toupper(str) }
let s:N.ToLower = { str -> tolower(str) }



" SubFirstWord :: str -> str -> str
let s:N.SubFirstWord = s:N.Substitute('')( '\v^\w*' )
let s:N.SetLine      = { line_nr -> { string -> setline(line_nr, string) } }
let s:N.SetThisLine  = s:N.SetLine('.')

" untested but probably works
" let s:N.AppendLine     = { line_nr -> { string -> setline( line_nr, string . getline(line_nr) ) } }
" let s:N.AppendThisLine = s:N.AppendLine('.')

" ChangeThisLine :: ( str -> str ) -> ()
let s:N.ChangeThisLine = { f -> s:N.SetThisLine( f( s:N.ThisLine() ) ) }

" let s:N.AppendCommentToThisLine = { comment -> s:N.ChangeThisLine( s:N.Substitute('')( '\s*#.*' ) ) }

" ifElse :: (a -> Boolean) -> (a -> b) -> (a -> b) -> a -> b
let s:N.IfElse = { cond -> { fna -> { fnb -> { a -> cond(a) ? fna(a) : fnb(a) } }}}
let s:N.LazyIfElse = { cond -> { fna -> { fnb -> { a -> {  -> cond(a) ? fna(a) : fnb(a) }}}}}
" when :: (a -> Boolean) -> (a -> a) -> a -> a
let s:N.When = { cond -> { fna -> { a -> cond(a) ? fna(a) : a }}}
let s:N.LazyWhen = { cond -> { fna -> { a -> { -> cond(a) ? fna(a) : a }}}}
let s:N.Unless = { cond -> { fna -> { a -> cond(a) ? a : fna(a) }}}
let s:N.LazyUnless = { cond -> { fna -> { a -> {  -> cond(a) ? a : fna(a)  }}}}
let s:N.IsBlank = { str -> str == '' }
" GoToLink :: link(string) -> () | ''
let s:N.GoToLink = s:N.Unless( s:N.IsBlank )( s:N.Chrome )
" let s:N.LazyGoToLink = 
" let s:N.Get    = { index -> { ary -> get(ary,index) }}
" let s:N.Index  = { int  -> { ary -> index(ary,int) } }
" let s:N.Len    = { expr -> len(expr) }
" let s:N.Modulo = { x -> { y -> x % y } }

" readfile({fname} [, {type} [, {max}]])
" writefile({object}, {fname} [, {flags}])

" s:N.ReadFile :: type -> max -> path
" probably do a WriteFileAndPath first
" or just do that in issues, if I want to retain the error behaviour for a
" file that doesn't exist
let s:N.ReadFile = { type -> { max -> { path -> max != 'no' ? readfile(path,type,max) : readfile(path,type) } } }
" echo s:N.ReadFile('')('no')(glob('~/.vimrc'))

let s:N.FileReadable = { path -> filereadable(path) }
" echo s:N.FileReadable(glob('~/.vimrc'))

" s:N.WriteFile :: flags -> path -> ary
let s:N.WriteFile = { flags -> { path -> { ary -> [ writefile(ary,path,flags), path ][1] } } }
" echo s:N.WriteFile('a')( $HOME . '/.vim/hello_code_bear.txt' )([ 'heelllllo!' ])

" currently returns boolean... how to get it to return string
" very stupid and hacky way to do it, but I guess it does the job... would
" probably be nicer just to use fu! so I can have more than one line
" s:N._MakeDir :: flags -> dirname -> dirname
let s:N._MakeDir = { flags -> { dirname -> [ mkdir(dirname,flags), dirname ][1] } }

" s:N.IsDirectory :: path -> bool
let s:N.IsDirectory = { path -> isdirectory(path) }

" s:N.MakeDir :: dirname -> dirname
let s:N.MakeDir = s:N.Unless( s:N.IsDirectory )( s:N._MakeDir('p') )
" echo s:N.MakeDir($HOME.'/.vim/codebeartestdir/nesteddir')

" s:N.GetPathHead :: path -> pathhead
let s:N.GetPathHead = { path -> '/'.((path)->split('/')[0:-2]->join('/')) }

" s:N.GetPathTail :: path -> pathhead
let s:N.GetPathTail = { path -> (path->split('/')[-1]) }

" s:N.WriteFileAndPath :: flags -> path -> ary -> path
let s:N.WriteFileAndPath = { flags -> { path -> { ary -> s:N.WriteFile(flags)( s:N.MakeDir( s:N.GetPathHead(path) ).'/'.s:N.GetPathTail(path) )(ary)  }} }
"echo s:N.WriteFileAndPath('a')( $HOME . '/.vim/somenonexistentdir/hello_code_bear.txt' )([ 'heelllllo!' ])

" could become memory intensive, as we're loading the whole file into memory
" just to be able to prepend
fu! _PrependToFile( path, ary )
  return s:N.WriteFileAndPath( '' )( a:path )( a:ary + (filereadable(a:path) ? readfile(a:path) : []) )
endfu

let s:N.PrependToFile = { path -> { ary -> _PrependToFile( path, ary ) } }

let s:N.Split    = { sep -> { ary -> split(ary, sep) } }
let s:N.Words    = { ary -> split(ary, ' ') }
let s:N.Lines    = { ary -> split(ary, "\n") }
let s:N.Join     = { sep -> { ary -> join(ary, sep) } }
let s:N.Unwords  = { ary -> join(ary, ' ') }
let s:N.Unlines  = { ary -> join(ary, "\n") }
let s:N.Tail     = { ary -> ary[1:-1] }
let s:N.Head     = { ary -> ary[0] }
let s:N.HeadTail = { ary -> [ ary[0], ary[1:-1] ] }

" let s:N.GitBranchName = { -> s:N.Head( s:N.Lines( s:N.System('git branch --show-current') ) ) }
" let s:N.GitLastTwentyCommits = { -> s:N.Lines( s:N.System('git log --pretty=oneline | head -n 20') ) }

let s:N.SetFT = { ft -> { bufnr -> s:N.Snd([ setbufvar( winbufnr( bufnr ), '&filetype', ft), bufnr ]) } }
let s:N.Popup = { options -> { text -> popup_create( text, options() ) } }
let s:N.ClosePopup = { bufnr -> popup_close( bufnr ) }

let s:N.PopupTL = { title -> s:N.Popup({ -> { 'title':     ' '.title.' '
                                          \ , 'padding':   [1,1,1,1]
                                          \ , 'line':      1
                                          \ , 'col':       &columns
                                          \ , 'pos':       'topright'
                                          \ , 'scrollbar': 1
                                          \ }})}

let s:N.PopupTLNoTitle = s:N.Popup({ -> { 'padding':   [1,1,1,1]
                                 \ , 'line':      1
                                 \ , 'col':       &columns
                                 \ , 'pos':       'topright'
                                 \ , 'scrollbar': 1
                                 \ }})

let s:N.PopupCNoTitle = s:N.Popup({ -> { 'padding':   [1,1,1,1]
                                 \ , 'line':      1
                                 \ , 'col':       &columns
                                 \ , 'pos':       'center'
                                 \ , 'scrollbar': 1
                                 \ , 'moved':     'any'
                                 \ }})

" these shouldn't actually contain the title
let s:N.RubyPopupTL     = s:N.C( s:N.SetFT('ruby')     )( s:N.PopupTLNoTitle )
let s:N.MarkdownPopupTL = s:N.C( s:N.SetFT('markdown') )( s:N.PopupTLNoTitle )
let s:N.LogPopupTL      = s:N.C( s:N.SetFT('log')      )( s:N.PopupTLNoTitle )

let s:N.PopupClear = { -> popup_clear() }

" call popup_clear()
" call s:N.MarkdownPopupTL( ['some title', '----------', '- lkjsdlfk', '- klsdjflksdf'] )

" fu! TopRightPopup( title, text, ft ) " text can be an array or string
"     let g:top_right_popup = popup_create(
"                 \ a:text,
"                 \ {'title': ' '.a:title.' ',
"                 \ 'padding': [1,1,1,1],
"                 \ 'line': 1,
"                 \ 'col': &columns,
"                 \ 'pos':'topright',
"                 \ 'border':[1,1,1,1],
"                 \ 'scrollbar': 1})
"     call setbufvar( winbufnr(g:top_right_popup), '&filetype', a:ft)
" endfu

let s:N.SystemToQFList = g:NyaoFn.FoldCompose([ g:NyaoFn.SetQFList
                                            \ , g:NyaoFn.Map({ line -> {"filename": line} })
                                            \ , g:NyaoFn.Lines
                                            \ , g:NyaoFn.System
                                            \ ])

let s:N.Cwd = { -> getcwd() }

" stolen from abolish.vim
function! SnakeCase(word)
  let word = substitute(a:word,'::','/','g')
  let word = substitute(word,'\(\u\+\)\(\u\l\)','\1_\2','g')
  let word = substitute(word,'\(\l\|\d\)\(\u\)','\1_\2','g')
  let word = substitute(word,'[.-]','_','g')
  let word = tolower(word)
  return word
endfunction

let s:N.SnakeCase = { str -> SnakeCase(str) }

" could probably be improved to capitalize each word but... whatever
let s:N.Capitalize = { word -> substitute(word, '\v(.)', '\u\1', '') }


" Buffers

let s:N.GetBufInfo = { opts -> getbufinfo(opts) }
let s:N.GetAllBufInfo = { -> getbufinfo() }

fu! s:_DeleteBuffer(num)
  exe "normal! :bd ".a:num."\<CR>"
  return a:num
endfu

fu! s:_WipeoutBuffer(num)
  exe "normal! :bwipeout ".a:num."\<CR>"
  return a:num
endfu

let s:N.DeleteBuffer = { num -> s:_DeleteBuffer(num) }
let s:N.WipeoutBuffer = { num -> s:_WipeoutBuffer(num) }


" Async {{{1
" ---------

let s:EachLineHandler = { channel, msg -> s:N.Echom(msg) }

" it would be good if close handler could get the full output
let s:CloseHandler = { channel -> s:N.Echom('Job done, melord!') }
let s:ErrorHandler = { channel, data -> s:N.Echoerr(data) }

" this appears to be kind enough to increment the channel even for the same
" command, so I think I can use that to manage and accumulate state
" let job = job_start(['sh', '-c', 'ls'], {'out_cb': s:EachLineHandler, 'close_cb': s:CloseHandler, 'err_cb': s:ErrorHandler })
" let job = job_start(['sh', '-c', 'ls'], {'out_cb': s:EachLineHandler, 'close_cb': s:CloseHandler, 'err_cb': s:ErrorHandler })
" let job = job_start(['sh', '-c', 'ls'], {'out_cb': s:EachLineHandler, 'close_cb': s:CloseHandler, 'err_cb': s:ErrorHandler })
" let job = job_start(['sh', '-c', 'ls'], {'out_cb': s:EachLineHandler, 'close_cb': s:CloseHandler, 'err_cb': s:ErrorHandler })

let s:N.ShJob = { ErrorHandler -> { CloseHandler -> { LineHandler -> { cmd ->
  \ job_start(['sh', '-c', cmd], {'out_cb': LineHandler, 'close_cb': CloseHandler, 'err_cb': ErrorHandler })
\ } } } }

let s:N.SimpleJob = s:N.ShJob( s:ErrorHandler )

" maybe the most functional way to do this is to pass in the locks obj, rather
" than getting it from global state... such that any plugin could use whatever
" name it wants for the locks without conflicting, and things that need to
" share a set of locks could explicitly do so
" not sure what functional programming thinks about local state... is it
" always completely stateless, or is it just important that a function always
" return the same result for the same input?
" maybe I need to port Future to vimscript T.T
" the main difference between this and what I'm used to is that I'm getting
" the handler callback multiple times rather than just once... and there's
" something I want to do each time piecemeal, but others that I only want to
" do once it is done. normally there is just cb, fb, but in this case there's
" cb, fb, and exit callback, where you have to accumulate some value in the cb
" if you want to use it in the exit... so Future doesn't really solve that
" problem at all, it almost entirely requires some kind of state if you need
" to do something in the exit callback with the data.
"
" I think it would also be pretty cool to have a version of  SimpleJob that
" did this for you... Or maybe it should always do this, to make life easier,
" such that the LineHandler you give it, if you give it any, will handle
" accumulation for you and then pass on that data to the CloseHandler... or
" both will be a wrapper, and they will call your functions with the data
" only, while in the background handling the channel part... eg

let s:N.job_channels = {}

let s:ExtractChannelNr = s:N.C( s:N.Snd )( s:N.Words )

let s:PushJobChannelState = { channel -> { line -> add(s:N.job_channels[s:ExtractChannelNr(channel)], msg) } }

" it would be nice to have a case statement matcher... not sure how that would
" work. maybe an array of pairs, matcher and f()
let s:MaybeGetJobState = { channel -> 
      \ s:N.IfElse(
        \ { channel_nr -> has_key(s:N.job_channels, channel_nr) }
      \ )(
        \ { channel_nr -> s:N.maybe( s:N.job_channels[ channel_nr ] ) }
      \ )(
        \ { _ -> s:N.Nothing }
      \ )( 
        \ s:ExtractChannelNr(channel) 
      \ )
    \ }

" note to self && will always return 1 or 0, it is not a guard statement
" echo has_key(s:N.job_channels, '1') && 'test' == 'test'
" echo s:MaybeGetJobState('channel 4 open')

let s:SafePushJobChannelState = { channel -> { line ->  
      \ s:N.IfElse(
        \ { channel_nr -> !has_key(s:N.job_channels, channel_nr) }
      \ )(
        \ { channel_nr -> s:N.LetObj(s:N.job_channels)(channel_nr)([line]) }
      \ )(
        \ { channel_nr -> add(s:N.job_channels[channel_nr], line) }
      \ )( 
        \ s:ExtractChannelNr(channel) 
      \ )
      \ }}

" we make a copy so we can release the original from memory and the copy can
" be collected by GC when there's no references to it
"
" the error handler also needs to do this...
" although the error handler gets called multiple times, so maybe that also
" makes it hard? I don't know if close handler will always be the last thing
" called. Slightly harder to do this than I thought, rspec giving me lots of
" edge cases
"
" even with this fix... not working...
" maybe the close handler is what dies?

" it probably is failing here...
let s:CloseHandlerWithState = { f -> { channel ->
      \   s:N.Sequence([ {  -> f( copy(s:MaybeGetJobState(channel)) ) } 
                     \ , {  -> s:N.Unlet('g:NyaoFn.job_channels['.s:ExtractChannelNr(channel).']')}])}}

let s:EachLineHandlerWithState = { f -> { channel, msg  -> 
  \ s:N.Sequence([ { -> s:SafePushJobChannelState( channel )( msg ) }
               \ , { -> f( s:MaybeGetJobState(channel), msg ) }])}}

let s:ErrorHandlerWithState = { f -> { channel, data -> f( copy(s:MaybeGetJobState(channel)), data ) }}

let s:N.ShJobWithState = { ErrorHandler -> { CloseHandler -> { LineHandler -> { cmd ->
  \ job_start( ['sh', '-c', cmd]
           \ , {'out_cb':  s:EachLineHandlerWithState(LineHandler)
           \ , 'close_cb': s:CloseHandlerWithState(CloseHandler)
           \ , 'err_cb':   s:ErrorHandlerWithState(ErrorHandler) })}}}}
"
let s:N.SimpleJobWithState = s:N.ShJobWithState({ state, data -> s:N.Echoerr(data) })

" call s:N.ShJobWithState( { state, data -> state })({ state -> s:N.Echom(state) })({ state, line -> s:N.Echom(state) })('ls')
" call s:N.SimpleJobWithState({ state -> s:N.Echom(state) })({ state, line -> s:N.Echom(state) })('ls')

"
" let s:N.SimpleJobWithLock = { lock_name -> ... IfElse do job }
"
" Lock could be more generic... it doesn't have to be about a job


" the only real improvement to this would be to write it to a file instead, or
" redis or whatever so it could be used across instances or different
" utilities but... I don't ever use vim in that way and it doesn't seem that
" great to me
let s:N.LockDo = { locker -> { lock_name -> { f ->
      \ s:N.IfElse(
          \ { lock_name -> has_key(locker, lock_name) }
        \ )(
          \ { lock_name -> s:N.Echoerr(lock_name . " lock already locked.") }
        \ )(
          \ { lock_name -> s:N.Sequence([ { -> s:N.LetObj(locker)(lock_name)(1) }, f]) }
        \ )( lock_name )
      \ } } }

let s:N.SilentLockDo = { locker -> { lock_name -> { f ->
      \ s:N.Unless(
          \ { lock_name -> has_key(locker, lock_name) }
        \ )(
          \ { lock_name -> s:N.Sequence([ { -> s:N.LetObj(locker)(lock_name)(1) }, f]) }
        \ )( lock_name )
      \ } } }

let s:N.Unlock = { locker_name -> { lock_name -> s:N.Unlet(locker_name.'["'.lock_name.'"]') } }

" so, to use this with async it would be something like the below
" ... which works just fine... I'm happy enough with that. Now I just put that
" in my onwrite hook, and I can know that if I save a bunch of times in a row,
" I won't stack up processes for style or test. you would release the lock in
" the close callback, in normal circumstances
" I could make a wrapper around both my types of jobs to compose with lock

" let s:myasynclocker = {}
" let s:MyAsyncLock = s:N.LockDo(s:myasynclocker)('myasynclock')
" let s:MyAsyncUnlock = {  -> s:N.Unlock('s:myasynclocker')('myasynclock') }

" let s:MyAsyncJob = { -> s:N.SimpleJobWithState({ state -> s:N.Echom(state) })({ state, line -> s:N.Echom(state) })('sleep 5; ls') }
" call s:MyAsyncLock( s:MyAsyncJob )
" call s:MyAsyncUnlock()

" call s:MyAsyncLock( s:MyAsyncJob )
" call s:MyAsyncUnlock()
" call s:MyAsyncLock( s:MyAsyncJob )
" call s:MyAsyncUnlock()
" call s:MyAsyncLock( s:MyAsyncJob )

" let s:locker = {}
" call s:N.LockDo(s:locker)('beans')({ -> s:N.Echo('beans+!') })
" call s:N.Unlock('s:locker')('beans')

" call s:N.LockDo(s:locker)('beans')({ -> s:N.Echo('beans+!') })
" call s:N.Unlock('s:locker')('beans')

" call s:N.LockDo(s:locker)('beans')({ -> s:N.Echo('beans+!') })
" echo s:locker
" let s:locker = {}

" call s:N.ShJob( ErrorHandler )( CloseHandler )( EachLineHandler )( 'ls' )
" }}}1
" signs {{{
" -----

let s:N.SignPlace = { name -> { group -> { id -> { buf -> { options ->
      \ sign_place(id, group, name, buf, options)
\ } } } } }

let s:N.SignUnplace = { group -> { options -> sign_unplace(group, options) } }

let s:N.SignDefine = { options -> { name -> sign_define(name, options) } }

" call s:N.SignDefine( {"text" : "=>", "texthl" : "RedText", "linehl" : "RedText"})('rubocopsign')
" call s:N.SignPlace('rubocopsign')('rubocop')(0)(bufname())({'lnum':762, 'priority':10})
" call s:N.SignUnplace( 'rubocop' )( {} )

" }}}
" thoughts {{{
" ----------------------------------------------------------- "
" a monad is just a monoid is the category of endofunctors... "
" ----------------------------------------------------------- "

" Thoughts
" --------
" Especially when using compose, writing the type signatures in comments is
" pretty important... it's very easy to forget how many arguments something
" needs after it has been composed. For normal definitions, that type is
" usually revealed in the lamda
"
" Real functional programmers (eg: not me) rely on a feedback loop of compile
" errors from type signatures. We could try to achieve something similar with
" a more complex way of defining lamdas, which would include an array of
" expected types but... these old bones ache.
"
" For now, just make liberal use of Kozou, don't make assumptions about how
" things work since vim has lots have very weird gotchas
" }}}
" tests {{{

if !exists('g:T')
  finish
endif

let g:T.dont_run = 1
call T.reset()
call Te( "string returns type 1", 0, s:N.Type, ['string'], T.is(1))
call Te( "to string on number returns string of number", 0, s:N.ToString, [10], T.is('10'))
call Te( "fold compose works in correct direction", 0,
      \ s:N.FoldCompose([ s:N.Type, s:N.ToString  ]),
      \ [100], T.is(1))
call Te( "s:N.MatchStrBool", 0, s:N.MatchStrBool('test'), ['test'], T.is(1))
call Te( "s:N.MatchStrBool", 0, s:N.MatchStrBool('test'), ['lskdkltest'], T.is(1))
call Te( "s:N.MatchStrBool", 0, s:N.MatchStrBool('test'), ['testilskdl'], T.is(1))
call Te( "s:N.MatchStrBool", 0, s:N.MatchStrBool('test'), ['tedst'], T.is(0))
call Te( "s:N.MatchStrBool", 0, s:N.MatchStrBool('test'), [''], T.is(0))

" call Te( 'A is A', 'A', 0, 'noop', T.is('A'))
" call Te( 'A is B', 'A', 0, 'noop', T.is('B'))
call Te( 'A is 0', 'A', 0, 'noop', T.is(0)) " so... here's what's confusing... some kind of coersion is happening
call Te( 'v:null is 0', v:null, 0, 'noop', T.is(0))

call Te( "s:N.Find", 0, s:N.Find( { a -> a == 'A' }), [[1, 2, 'A']], T.is('A') )
call Te( "s:N.Find", 0, s:N.Find( { a -> a == 'A' }), [[1, 2, 'B']], T.propIs('name', 'Nothing') )
call Te( "s:N.Find", 0, s:N.Find( { a -> a == 'A' }), [[1, 2]],     T.propIs('name', 'Nothing') )

call Te( "s:N.MaybeFind", 0, s:N.MaybeFind( { a -> a == 'A' }), [[1, 2]], T.isNothing() )

call Te( "s:N.Contains { a -> a == 'A' }", 0, s:N.Contains( { a -> a == 'A' }), [[1, 2]], T.is(0) )
call Te( "s:N.Contains { a -> a == 'A' }", 0, s:N.Contains( { a -> a == 'A' }), [[1, 2, 'A']], T.is(1) )

call Te( "s:N.FindStrInAry 'test'", 0, s:N.FindStrInAry('test'), [ ['1', 'test', 5] ], T.is(1))
call Te( "s:N.FindStrInAry 'test'", 0, s:N.FindStrInAry('test'), [ ['1', 'tesdflkjst', 5] ], T.is(0))

call Te( "s:N.ToUpper 'test'", 0, s:N.ToUpper, [ 'test' ], T.is('TEST'))
call Te( "s:N.ToLower 'test'", 0, s:N.ToLower, [ 'TEST' ], T.is('test'))
call Te( "s:N.Fst", 0, s:N.Fst, [ ['a', 'b'] ], T.is('a'))
call Te( "s:N.Snd", 0, s:N.Snd, [ ['a', 'b'] ], T.is('b'))
call Te( "s:N.Split ' '", 0, s:N.Split(' '), [ 'blah blah blah' ], T.is(['blah', 'blah', 'blah']))
call Te( "s:N.Head", 0, s:N.Head, [ [ 'a', 'b', 'c'] ], T.is('a'))
call Te( "s:N.Tail", 0, s:N.Tail, [ [ 'a', 'b', 'c'] ], T.is([ 'b', 'c' ]))
call Te( "s:N.Take(2)", 0, s:N.Take(2), [ ['a','b','c'] ], T.is(['a','b']))
call Te( "s:N.Drop(2)", 0, s:N.Drop(2), [ ['a','b','c'] ], T.is(['c']))
call Te( "s:N.Eq('a')", 0, s:N.Eq('a'), [ 'a' ], T.is(1))
call Te( "s:N.Eq('a')", 0, s:N.Eq('a'), [ 'b' ], T.is(0))

call Te( "s:N.TakeWhile(s:N.Eq('a'))",    0, s:N.TakeWhile(s:N.Eq('a')),    [ ['a', 'a', 'c'] ], T.is(['a', 'a']))
call Te( "s:N.TakeWhile(s:N.Eq('a'))",    0, s:N.TakeWhile(s:N.Eq('a')),    [ ['b', 'a', 'c'] ], T.is([]))
call Te( "s:N.TakeWhile(s:N.NotEq('a'))", 0, s:N.TakeWhile(s:N.NotEq('a')), [ ['b', 'a', 'c'] ], T.is(['b']))

call Te( "s:N.DropWhile(s:N.Eq('a'))",    0, s:N.DropWhile(s:N.Eq('a')),    [ ['a', 'a', 'c'] ], T.is(['c']))
call Te( "s:N.DropWhile(s:N.Eq('a'))",    0, s:N.DropWhile(s:N.Eq('a')),    [ ['b', 'a', 'c'] ], T.is(['b', 'a', 'c']))
call Te( "s:N.DropWhile(s:N.NotEq('a'))", 0, s:N.DropWhile(s:N.NotEq('a')), [ ['b', 'a', 'c'] ], T.is(['a', 'c']))

call Te( "s:N.DropTakeWhile(s:N.Eq('a'))", 0, s:N.DropTakeWhile(s:N.Eq('a')), [ ['a', 'a', 'c'] ], T.is([ ['a', 'a'], ['c'] ]))
call Te( "s:N.DropTakeWhile(s:N.Eq('a'))", 0, s:N.DropTakeWhile(s:N.Eq('a')), [ ['b', 'a', 'c'] ], T.is([ [],['b', 'a', 'c'] ]))
call Te( "s:N.DropTakeWhile(s:N.NotEq('a'))", 0, s:N.DropTakeWhile(s:N.NotEq('a')), [ ['b', 'a', 'c'] ], T.is([ ['b'],['a', 'c'] ]))

call Te( "s:N.SplitAryAt(s:N.Eq(''))", 0, s:N.SplitAryAt(s:N.Eq('')), [ ['b', '', 'c'] ], T.is([ ['b'],['c'] ]))
call Te( "s:N.SplitAryAt(s:N.Eq(''))", 0, s:N.SplitAryAt(s:N.Eq('')), [ ['', 'b', 'c'] ], T.is([ ['b','c'] ]))
call Te( "s:N.SplitAryAt(s:N.Eq(''))", 0, s:N.SplitAryAt(s:N.Eq('')), [ ['', '', ''] ], T.is([]))

call Te( "s:N.Paragraphs)", 0, s:N.Paragraphs, [ ['b', '', 'c'] ], T.is([ ['b'],['c'] ]))

call Te( "s:N.GroupToObj(s:N.Prop('name'))", 0, s:N.GroupToObj(s:N.Prop('name')),
      \ [ [ {'name':'a', 'lvl':2}, {'name':'a','lvl':1}, {'name':'b'} ] ],
      \ T.is({ 'a': [{'name':'a', 'lvl':2}, {'name':'a','lvl':1}], 'b':[{'name':'b'}]}))

call Te( "s:N.GroupBy(s:N.Prop('name'))", 0, s:N.GroupBy({ a -> { b -> a.name == b.name } }),
      \ [ [ {'name':'a', 'lvl':2}, {'name':'a','lvl':1}, {'name':'b'} ] ],
      \ T.is([ [{'name':'a', 'lvl':2}, {'name':'a','lvl':1}], [{'name':'b'}]]))

call Te( "s:N.Sort({ a -> { b -> a > b } })", 0, s:N.Sort({ a -> { b -> a > b } }), [ ['b', 'a', 'c'] ], T.is(['a', 'b', 'c']))
call Te( "s:N.Sort({ a -> { b -> a < b } })", 0, s:N.Sort({ a -> { b -> a < b } }), [ ['b', 'a', 'c'] ], T.is(['c', 'b', 'a']))

call Te( "s:N.Flatten", 0, s:N.Flatten, [ [[ 'b' ], 'a', 'c'] ], T.is(['b', 'a', 'c']))
call Te( "s:N.Lens(['a','b'])", 0, s:N.Lens(['a','b']), [ {'a':{'b':'boo'}} ], T.is('boo'))
call Te( "s:N.Lens(['a','c'])", 0, s:N.Lens(['a','c']), [ {'a':{'b':'boo'}} ], T.propIs('name', 'Nothing'))

call Te( "s:N.MaybeLens(['a','c'])", 0, s:N.MaybeLens(['a','c']), [ {'a':{'b':'boo'}} ], T.isNothing())

call T.printResults()

" }}}
