/+  language-server-parser, pprint
::  Autocomplete for hoon.
::
=/  debug  |
|%
++  option
  |$  [item]
  [term=cord detail=item]
::
::  Like +rose except also produces line number
::
++  lily
  |*  [los=tape sab=rule]
  =+  vex=(sab [[1 1] los])
  ?~  q.vex
    [%| p=p.vex(q (dec q.p.vex))]
  ?.  =(~ q.q.u.q.vex)
    [%| p=p.vex(q (dec q.p.vex))]
  [%& p=p.u.q.vex]
::
::  Get all the identifiers accessible if this type is your subject.
::
++  get-identifiers
  |=  ty=type
  ^-  (list (option type))

  ~?  >  debug  %get-identifiers
  %-  flop
  |-    ^-  (list (option type))
  ?-    ty
      %noun      ~?  >  debug  %noun  ~
      %void      ~?  >  debug  %void  ~
      [%atom *]  ~?  >  debug  %atom  ~
      [%cell *]
    ~?  >  debug  %cell
    %+  weld
      $(ty p.ty)
    $(ty q.ty)
  ::
      [%core *]
    ~?  >  debug  %core
    %-  weld
    :_  ?.  ?=(%gold r.p.q.ty)
          ~
    $(ty p.ty)
    ^-  (list (option type))
    %-  zing
    %+  turn  ~(tap by q.r.q.ty)
    |=  [term =tome]
    %+  turn
      ~(tap by q.tome)
    |=  [name=term =hoon]
    ^-  (pair term type)
    ~|  term=term
    [name ~(play ~(et ut ty) ~[name] ~)]
  ::
      [%face *]
    ~?  >  debug  face+ty
    ?^  p.ty
      =/  aliases=(list (option type))
        %+  turn
          ~(tap by p.p.ty)
        |=  [name=term hun=(unit hoon)]
        =/  ale=vase  (~(mint ut ty) %noun (need hun))
        =/  dur=type  -:(slop ale !>(..zuse))
        [name dur]
      ~?  >  debug  aliases+aliases
      %+  welp  aliases
      |-  ^-  (list (option type))
      ?~  aliases  ~
      %+  welp  ^$(ty detail.i.aliases)
      $(aliases t.aliases)
    [p.ty q.ty]~
  ::
      [%fork *]
    ~?  >  debug  fork+p.ty
    %=    $
        ty
      =/  tines  ~(tap in p.ty)
      ?~  tines
        %void
      |-  ^-  type
      ?~  t.tines
        i.tines
      (~(fuse ut $(tines t.tines)) i.tines)
    ==
  ::
      [%hint *]
        ~?  >  debug  %hint
        $(ty q.ty)
      [%hold *]
        ~?  >  debug  %hold
        $(ty ~(repo ut ty))
  ==
::
::
++  search-exact
  |*  [sid=term options=(list (option type))]
  ^-  (option [type (unit tape)])
  =-  ?~  -
    :-  sid  [*type `"No type found."]
  :-  term.i.-  [+.i.- ~]
  ^+  options
  %+  skim  options
    |=  [id=cord *]
    =(sid id)

::
::  Get all the identifiers that start with sid.
::
++  search-prefix
  |*  [sid=cord ids=(list (option))]
  ^+  ids
  %+  skim  ids
  |=  [id=cord *]
  ^-  ?(%.y %.n)
  ~?  >  &(debug =(sid (end [3 (met 3 sid)] id)))  prefix+[sid id]
  =(sid (end [3 (met 3 sid)] id))
::
::  Get the longest prefix of a list of identifiers.
::
++  longest-match
  |=  matches=(list (option))
  ^-  cord
  ?~  matches
    ''
  =/  n  1
  =/  last  (met 3 term.i.matches)
  |-  ^-  term
  ?:  (gth n last)
    term.i.matches
  =/  prefix  (end [3 n] term.i.matches)
  ?:  |-  ^-  ?
      ?|  ?=(~ t.matches)
          ?&  =(prefix (end [3 n] term.i.t.matches))
              $(t.matches t.t.matches)
      ==  ==
    $(n +(n))
  (end [3 (dec n)] term.i.matches)
::
::  Run +find-type safely, printing the first line of the stack trace on
::  error.
::

++  find-type-mule
  |=  [sut=type gen=hoon]
  ^-  (unit [term type])
  =/  res  (mule |.((find-type sut gen)))
  ?-  -.res
    %&  p.res
    %|
      =/  err  (flop (scag 10 p.res))
      ?:  debug
        %-  (slog err)  ~
      ~
  ==
::
++  find-type-exact
  |=  [sut=type gen=hoon]
  ^-  (unit (option [type (unit tape)]))
  =/  res  (mule |.((find-type sut gen)))
  ?-  -.res
    %&  %+  bind  p.res
      |=([id=term typ=type] [id [typ ~]])
    %|
     =,  format
     =-  `[%error [*type `"{(of-wall -)}"]]
     %-  zing  ^-  (list wall)
    (turn (scag 10 p.res) |=(a=tank (wash 0^160 a)))

  ==
::
::  Get the subject type of the wing where you've put the "magic-spoon".
::
++  find-type
  |=  [sut=type gen=hoon]
  =*  loop  $
  |^
  ^-  (unit [term type])
  ?-    gen
      [%cnts [%magic-spoon ~] *]    `['' sut]
      [%cnts [%magic-spoon @ ~] *]  ~?  >  debug  ms+gen  `[i.t.p.gen sut]
      [%cnts [%magic-spoon @ *] *]
    %=  $
      sut      (~(play ut sut) wing+t.t.p.gen)
      t.p.gen  t.p.gen(t ~)
    ==
  ::
      [%cnts [%magic-fork @ ~] *]
    `['' (~(play ut sut) wing+t.p.gen)]
  ::
      [^ *]      (both p.gen q.gen)
      [%brcn *]  (grow q.gen)
      [%brpt *]  (grow q.gen)
      [%cnts *]
    ?^  a=(find [%magic-spoon ~] p.gen)
      $(p.gen (slag u.a p.gen))
    |-  ^-  (unit [term type])
    =*  inner-loop  $
    ?~  q.gen
      ~
    %+  replace
      loop(gen q.i.q.gen)
    |.  inner-loop(q.gen t.q.gen)
  ::
      [%dtkt *]  (spec-and-hoon p.gen q.gen)
      [%dtls *]  loop(gen p.gen)
      [%rock *]  ~
      [%sand *]  ~
      [%tune *]  ~
      [%dttr *]  (both p.gen q.gen)
      [%dtts *]  (both p.gen q.gen)
      [%dtwt *]  loop(gen p.gen)
      [%hand *]  ~
      [%ktbr *]  loop(gen p.gen)
      [%ktls *]  (both p.gen q.gen)
      [%ktpm *]  loop(gen p.gen)
      [%ktsg *]  loop(gen p.gen)
      [%ktwt *]  loop(gen p.gen)
      [%note *]  loop(gen q.gen)
      [%sgzp *]  (both p.gen q.gen)
      [%sggr *]  loop(gen q.gen)  ::  should check for hoon in p.gen
      [%tsgr *]  (change p.gen q.gen)
      [%tscm *]
    %+  replace
      loop(gen p.gen)
    |.(loop(gen q.gen, sut (~(busk ut sut) p.gen)))
  ::
      [%wtcl *]  (bell p.gen q.gen r.gen)
      [%fits *]  (both p.gen wing+q.gen)
      [%wthx *]  loop(gen wing+q.gen)
      [%dbug *]  loop(gen q.gen)
      [%zpcm *]  (both p.gen q.gen)
      [%lost *]  loop(gen p.gen)
      [%zpmc *]  (both p.gen q.gen)
      [%zpts *]  loop(gen p.gen)
      [%zppt *]  (both q.gen r.gen)
      [%zpgl *]  (spec-and-hoon p.gen q.gen)
      [%zpzp *]  ~
      *
    =+  doz=~(open ap gen)
    ?:  =(doz gen)
      ~_  (show [%c 'hoon'] [%q gen])
      ~>  %mean.'play-open'
      !!
    loop(gen doz)
  ==
  ::
  ++  replace
    |=  [a=(unit [term type]) b=(trap (unit [term type]))]
    ^-  (unit [term type])
    ?~(a $:b a)
  ::
  ++  both
    |=  [a=hoon b=hoon]
    (replace loop(gen a) |.(loop(gen b)))
  ::
  ++  bell
    |=  [a=hoon b=hoon c=hoon]
    %+  replace  loop(gen a)
    |.  %+  replace  loop(gen b, sut (~(gain ut sut) a))
    |.  loop(gen c, sut (~(lose ut sut) a))
  ::
  ++  spec-and-hoon
    |=  [a=spec b=hoon]
    (replace (find-type-in-spec sut a) |.(loop(gen b)))
  ::
  ++  change
    |=  [a=hoon b=hoon]
    ::  ?^  x=(find [%magic-spoon ~] p.gen)
    ::    $(p.gen (slag u.x p.gen))
    ::  |-  ^-  (unit [term type])
    ::  =*  inner-loop  $
    ::  ?~  b
    ::    ~
    ::  %+  replace
    ::    loop(gen a)
    ::  |.  inner-loop(q.gen t.q.gen)
    ::  ~?  >  debug  change-a+(render-hoon:pprint a)
    ::  ~?  >  debug  change-b+(render-hoon:pprint b)
    =/  played  (mule |.((~(play ut sut) a)))
    =;  yalp
      (replace loop(gen a) |.(loop(gen b, sut yalp)))
    ?-  -.played
      %&  p.played
      %|
        =/  err  (flop (scag 10 p.played))
        ?:  debug  ((slog err) sut)
        sut
    ==
  ::
  ++  grow
    |=  m=(map term tome)
    =/  tomes  ~(tap by m)
    |-  ^-  (unit [term type])
    =*  outer-loop  $
    ?~  tomes
      ~
    =/  arms  ~(tap by q.q.i.tomes)
    |-  ^-  (unit [term type])
    =*  inner-loop  $
    ?~  arms
      outer-loop(tomes t.tomes)
    %+  replace
      loop(gen q.i.arms, sut (~(play ut sut) gen))
    |.  inner-loop(arms t.arms)
  --
::
::  Not implemented yet.  I wonder whether we should modify types found
::  in spec mode such that if it's a mold that produces a type, it
::  should just display the type and not that it's technically a
::  function.
::
++  find-type-in-spec
  |=  [sut=type pec=spec]
  ^-  (unit [term type])
  ~?  >  debug  find-type-in-spec+(render-type:pprint sut)
  ~?  >  debug  find-type-in-spec+pec
  ~
::
::
++  get-id-sym
  |=  [pos=@ud =tape]
  %^  get-id  pos  tape
  ^-  $-(nail (like (unit @t)))
  ;~(sfix (punt (cook crip ;~(pose dun (star alp)))) (star ;~(pose prn (just `@`10))))
::
++  get-id-cord
   |=  [pos=@ud =tape]
   %^  get-id  pos  tape
   ^-  $-(nail (like (unit @t)))
   ;~(sfix (punt (cook crip (star prn))) (star ;~(pose prn (just `@`10))))
::
++  get-id
  |=  [pos=@ud txt=tape seek=$-(nail (like (unit @t)))]
  ^-  [forward=(unit @t) backward=(unit @t) id=(unit @t)]
  =/  forward=(unit @t)
    (scan (slag pos txt) seek)
  =/  backward=(unit @t)
    %-  (lift |=(t=@t (swp 3 t)))
    (scan (flop (scag pos txt)) seek)
  =/  id=(unit @t)
    ?~  forward
      ?~  backward
        ~
      `u.backward
    ?~  backward
      `u.forward
    `(cat 3 u.backward u.forward)
  [forward backward id]
::
::  Insert magic marker in hoon source at the given position.
::
++  insert-magic
  |=  [pos=@ud txt=tape]
  ^-  [back-pos=@ud fore-pos=@ud txt=tape]
  ::  Find beg-pos by searching backward to where the current term
  ::  begins
  =+  (get-id-sym pos txt)
  =/  back-pos
    ?~  backward
      pos
    (sub pos (met 3 u.backward))
  =/  fore-pos
    ?~  forward
      pos
    (add pos (met 3 u.forward))
  :+  back-pos  fore-pos
  ::  Insert "magic-spoon" marker so +find-type can identify where to
  ::  stop.
  ::

  ;:  welp
    (scag back-pos txt)
    ?:  &(?=(~ id) ?=([%'.' *] (slag pos txt)))
      "magic-fork"
    ?:  &(!=(~ id) ?=([%'=' *] (slag fore-pos txt)))
      ;:  welp
        ?~  id  ""
        (weld (trip u.id) "=magic-spoon.")
        (slag (add fore-pos 1) txt)
        "\0a"
      ==
    ;:  welp
    "magic-spoon"
    ?~  id
      ""
    "."
    (slag back-pos txt)
    "\0a"
    ==
  ==

::
::  Produce the longest possible advance without choosing between
::  matches.
::
::    Takes a +hoon which has already has a magic-spoon marker.  Useful if
::    you want to handle your own parsing.
::
++  advance-hoon
  |=  [sut=type gen=hoon]
  %+  bind  (find-type-mule sut gen)
  |=  [id=term typ=type]
  =/  matches=(list (option type))
    (search-prefix id (get-identifiers typ))
  (longest-match matches)
::
::  Same as +advance-hoon, but takes a position and text directly.
::
++  advance-tape
  |=  [sut=type pos=@ud code=tape]
  =/  magicked  txt:(insert-magic pos code)
  ~?  >  debug  [%advance magicked]
  (advance-hoon sut (scan magicked vest))
::
++  exact-list-tape
  |=  [sut=type pos=@ud code=tape]
  ^-  (each (unit (option [type (unit tape)])) [row=@ col=@])
  ~?  >  debug  %start-magick
  =/  magicked  txt:(insert-magic pos code)
  ~?  >  debug  [%start-parsing magicked]
  ~?  >  debug  %start-parsing
  =/  res  (lily magicked (language-server-parser *path))
  ?:  ?=(%| -.res)
    ~?  >  debug  [%parsing-error p.res]
    ~?  >  debug  [%parsing-error magicked]
    [%| p.res]
  :-  %&
  ~?  >  debug  %parsed-good
  ((cury exact-list-hoon sut) hoon:`pile:clay`p.res)

::
::  Produce a list of matches.
::
::    Takes a +hoon which has already has a magic-spoon marker.  Useful if
::    you want to handle your own parsing.
::
++  tab-list-hoon
  |=  [sut=type gen=hoon]
  ^-  (unit (list (option type)))
  %+  bind  (find-type-mule sut gen)
  |=  [id=term typ=type]
  (search-prefix id (get-identifiers typ))
::
++  exact-list-hoon
  |=  [sut=type gen=hoon]
  ^-  (unit (option [type (unit tape)]))
  =/  exact  (find-type-exact sut gen)
  %+  bind  exact
  |=  in=[id=term [typ=type rep=(unit tape)]]
  ?^  rep.in  in
  =,  in
  ~!  exact
  ~!  typ.in
  ~!  typ
  (search-exact id (get-identifiers typ))
::  Same as +advance-hoon, but takes a position and text directly.
::
++  tab-list-tape
  |=  [sut=type pos=@ud code=tape]
  ^-  (each (unit (list (option type))) [row=@ col=@])
  ~?  >  debug  %start-magick
  =/  magicked  txt:(insert-magic pos code)
  ~?  >  debug  [%start-parsing magicked]
  ~?  >  debug  %start-parsing
  =/  res  (lily magicked (language-server-parser *path))
  ?:  ?=(%| -.res)
    ~?  >  debug  [%parsing-error p.res]
    ~?  >  debug  [%parsing-error magicked]
    [%| p.res]
  :-  %&
  ~?  >  debug  %parsed-good
  ((cury tab-list-hoon sut) hoon:`pile:clay`p.res)
::
++  tape-to-hoon
  |=  [sut=type pos=@ud code=tape]
  ^-  hoon
  ~?  >  debug  %start-magick-docs
  =/  magicked  txt:(insert-magic pos code)
  ~?  >  debug  [%start-parsing-docs magicked]
  ~?  >  debug  %start-parsing-docs
  =/  res  (lily magicked (language-server-parser *path))
  ?:  ?=(%| -.res)
    ~?  >  debug  [%parsing-error-docs p.res]
    *hoon
  ~?  >  debug  %parsed-good-docs
  hoon:`pile:clay`p.res
::
:: Generators
++  tab-generators
  |=  [pfix=path app=(unit term) gens=(list term)]
  ^-  (list (option tank))
  %+  turn  gens
  |=  gen=term
  ^-  (option tank)
  =/  pax=path
    (weld pfix ~[gen %hoon])
  =/  file
    .^(@t %cx pax)
  :_  (render-help file)
  ?~  app
    (cat 3 '+' gen)
  ?:  =(%hood u.app)
    (cat 3 '|' gen)
  :((cury cat 3) ':' u.app '|' gen)
::  Stolen from +help
++  render-help
  |=  a=@t
  ^-  tank
  :-  %leaf
  =/  c  (to-wain:format a)
  ?~  c  "~"
  ?.  =('::  ' (end [3 4] i.c))
    "<undocumented>"
  (trip i.c)
--
