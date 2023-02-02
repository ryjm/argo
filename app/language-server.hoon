/-  lsp-sur=language-server, *sole
/+  *server, dbug, dprint,
    auto=language-server-complete,
    lsp-parser=language-server-parser,
    easy-print=language-server-easy-print,
    rs=language-server-rune-snippet,
    build=language-server-build,
    default-agent, verb
!:
=/  debug  |
|%
+$  card  card:agent:gall
+$  lsp-req
  $:  uri=@t
      $%  [%sync changes=(list change)]
          [%completion position]
          [%commit @ud]
          [%hover position]
      ==
  ==
::
+$  change
  $:  range=(unit range)
      range-length=(unit @ud)
      text=@t
  ==
::
+$  range
  $:  start=position
      end=position
  ==
::
+$  position
  [row=@ud col=@ud]
::
+$  state-zero
  $:  %0
      bufs=(map uri=@t buf=wall)
      builds=(map uri=@t =vase)
      ford-diagnostics=(map uri=@t (list diagnostic:lsp-sur))
      preludes=(map uri=@t type)
      pending=(list card)
  ==
+$  versioned-state
  $%
    state-zero
  ==
--
=|  state-zero
=*  state  -
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this      .
      lsp-core  +>
      lsp       ~(. lsp-core bowl)
      def       ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^+  on-init:*agent:gall
    ^-  (quip card _this)
    ~&  >  %lsp-init
    :_  this  :_  ~
    :*  %pass  /connect
        %arvo  %e
        %connect  [~ /'~language-server-protocol']  %language-server
    ==
  ::
  ++  on-save   !>(state)
  ++  on-load
    ^+  on-load:*agent:gall
    |=  old-state=vase
    ^-  (quip card _this)
    ~&  >  %lsp-upgrade
    [~ this(state !<(state-zero old-state))]
  ::
  ++  on-poke
    ^+  on-poke:*agent:gall
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+    mark  (on-poke:def mark vase)
          %language-server-rpc-notification
        (on-notification:lsp !<(all:notification:lsp-sur vase))
          %language-server-rpc-request
        (on-request:lsp !<(all:request:lsp-sur vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ?:  ?=([%primary ~] path)
      `this
    ?.  ?=([%http-response @ ~] path)
      (on-watch:def path)
    `this
  ++  on-leave  on-leave:def
  ++  on-peek  on-peek:def
  ++  on-agent  on-agent:def
  ++  on-arvo
    ^+  on-arvo:*agent:gall
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    =^  cards  state
      ?+  sign-arvo  (on-arvo:def wire sign-arvo)
        [%eyre %bound *]  `state
        [%clay *]  (handle-build:lsp wire +.sign-arvo)
        [%behn *]  ~?  >  debug  [%behn pending:state]  [pending:state state(pending done)]
      ==
    [cards this]
  ::
  ++  on-fail   on-fail:def
  --
::
|_  bow=bowl:gall
::
++  give-rpc-notification
  |=  res=out:notification:lsp-sur
  ^-  (list card)
  :_  ~
  [%give %fact ~[/primary] %language-server-rpc-notification !>(res)]
::
++  on-notification
  |=  not=all:notification:lsp-sur
  ^-  (quip card _state)
  =^  cards  state
    ?+  -.not  [~ state]
        %text-document--did-open    (handle-did-open +.not)
        %text-document--did-change  (handle-did-change +.not)
        %text-document--did-save    (handle-did-save +.not)
        %text-document--did-close   (handle-did-close +.not)
        %progress                   (handle-progress +.not)
        %exit                       handle-exit
    ==
  [cards state]
++  on-request
  |=  req=all:request:lsp-sur
  ^-  (quip card _state)
  =^  cards  state
    ?+  -.req  [~ state]
      %text-document--hover       ~?  >  debug  %loading  (handle-loading req)
      %text-document--hover-complete  ~?  >  debug  [%hover req]  (handle-hover req)
      %text-document--completion  (handle-completion req)
    ==
  [cards state]
::
++  get-subject
  |=  uri=@t
  ^-  type
  (~(gut by preludes) uri -:!>(..zuse))
::
++  handle-completion
  |=  com=text-document--completion:request:lsp-sur
  ^-  (quip card _state)
  :_  state
  %^  give-rpc-response  %text-document--completion  id.com
  =/  buf=wall
    (~(got by bufs) uri.com)
  =/  txt=tape
    (zing (join "\0a" buf))
  =/  pos
    (get-pos buf row.com col.com)
  =/  rune  (rs (swag [(safe-sub pos 2) 2] txt))
  ?^  rune  rune
  =/  tab-list
    %^  tab-list-tape:auto
      (~(gut by preludes) uri.com -:!>(..zuse))
    pos  txt
  ?:  ?=(%| -.tab-list)  ~
  ?~  p.tab-list  ~
  ?~  u.p.tab-list  ~
  (turn u.p.tab-list make-completion-item)
::
++  make-completion-item
  |=  [name=term =type]
  ^-  completion-item:lsp-sur
  =/  doc
    %-  crip
    ;:  weld
      "`"
      ~(ram re ~(duck easy-print type))
      "`"
    ==
  [name 1 doc '' name 1]
::
++  give-rpc-response
  |=  res=all:response:lsp-sur
  ^-  (list card)
  :_  ~
  [%give %fact ~[/primary] %language-server-rpc-response !>(res)]
::
++  handle-progress
  |=  pog=[title=@t kind=@t message=(unit @t) percentage=(unit @ud)]
  ^-  (quip card _state)
  ~?  >  debug  progress+pog
  [~ state]
::
++  handle-exit
  ^-  (quip card _state)
  ~&  >  %lsp-shutdown
  :_  *state-zero
  %+  turn
    ~(tap in ~(key by builds))
  |=  uri=@t
  [%pass /ford/[uri] %arvo %c %warp our.bow %base ~]
::
++  handle-did-close

  |=  [uri=@t version=(unit @)]
  ^-  (quip card _state)
  =.  bufs
    (~(del by bufs) uri)
  =.  ford-diagnostics
    (~(del by ford-diagnostics) uri)
  =.  builds
    (~(del by builds) uri)
  :_  state
  [%pass /ford/[uri] %arvo %c %warp our.bow %base ~]~
::
++  handle-did-save
  |=  [uri=@t version=(unit @)]
  ^-  (quip card _state)
  ~?  >  debug  handle-did-save+uri
  =/  =desk  (find-desk uri)
  ~?  >  debug  handle-did-save+desk
  :_  state
  =-  %+  weld  -  (give-rpc-notification (get-diagnostics uri))
  =-  (hite desk (uri-to-path:build uri) -)
   %-  crip  (zing (join "\0a" `wall`(~(got by bufs) uri)))
::
++  handle-did-change
  |=  [document=versioned-doc-id:lsp-sur changes=(list change:lsp-sur)]
  ^-  (quip card _state)
  =/  updated=wall
    (sync-buf (~(got by bufs) uri.document) changes)
  =.  bufs
    (~(put by bufs) uri.document updated)
  `state
::
++  handle-build
  |=  [=path =gift:clay]
  ^-  (quip card _state)
  ?>  ?=([%writ *] gift)
  =/  uri=@t
    (snag 1 path)
  =/  loc=^path  (uri-to-path:build uri)
  =;  [res=(quip card _state) dek=desk]
    [(snoc -.res (build-file | uri loc `dek)) +.res]
  ?~  p.gift
    [[~ state] %base]
  =.  builds
    (~(put by builds) uri q.r.u.p.gift)
  =.  ford-diagnostics
    (~(del by ford-diagnostics) uri)
  =/  bek  byk.bow(r da+now.bow)
  =/  desks=(list desk)  ~(tap in .^((set desk) %cd (en-beam bek /)))
  =|  dek=desk
  |-
  ?~  desks  [[~ state] %base]
  =.  dek  ?:  =(%kids i.desks)  %base  i.desks
  =/  exists=?  .^(? %cu (en-beam bek(q dek) loc))
  ?.  exists  $(desks t.desks)
  =+  .^(=open:clay %cs /(scot %p our.bow)/[dek]/(scot %da now.bow)/open/foo)
  =/  =type  -:(open loc)

  =.  preludes
    (~(put by preludes) uri type)
  :_  dek
  :_  state
  (give-rpc-notification (get-diagnostics uri))
::
++  get-diagnostics
  |=  uri=@t
  ^-  out:notification:lsp-sur
  :+  %text-document--publish-diagnostics
    uri
  %+  weld
    (~(gut by ford-diagnostics) uri ~)
  (get-parser-diagnostics uri)
::
++  build-file
  |=  [eager=? uri=@t =path desk=(unit desk)]
  ^-  card
  =/  =rave:clay
    ?:  eager
      [%sing %a da+now.bow path]
    [%next %a da+now.bow path]
  =/  des=^desk  ?^  desk  u.desk  %base
  [%pass /ford/[uri] %arvo %c %warp our.bow des `rave]
::
::  for finding a desk when saving a file
::  ignores files in %base
::  if there are multiple desks, picks the first one 
::  TODO allow user to specify which desk to use
++  find-desk
  |=  uri=@t
  =/  bek  byk.bow
  =/  =path
    (uri-to-path:build uri)
  =/  desks=(list desk)  ~(tap in .^((set desk) %cd (en-beam bek /)))
  =|  dek=desk
  |-
  ?~  desks  %$
  =.  dek  ?:  =(%kids i.desks)  %base  i.desks
  ::  to avoid saving to %base
  ?:  =(dek %base)  $(desks t.desks)
  =/  exists=?  .^(? %cu (en-beam bek(q dek) path))
  ?.  exists  $(desks t.desks)
  dek
++  handle-did-open
  |=  item=text-document-item:lsp-sur
  ^-  (quip card _state)
  ~?  >  debug  handle-did-open+uri.item
  =/  =path
    (uri-to-path:build uri.item)
  =/  bek  byk.bow
  =/  desks=(list desk)  ~(tap in .^((set desk) %cd (en-beam bek /)))
  =|  dek=desk
  |-
  ?~  desks  [~ state]
  =.  dek  ?:  =(%kids i.desks)  %base  i.desks
  =/  exists=?  .^(? %cu (en-beam bek(q dek) path))
  ?.  exists  $(desks t.desks)
  ?~  path  `state
  ~?  >  debug  path
  ?:  ?=([%lib %language-server @ @ ~] path)
    `state
  ?:  ?=(%sys -.path)
    `state
  ~?  >  debug  "%source-desk for {<path>}: {<dek>}"
  =/  buf=wall
    (to-wall (trip text.item))
  =.  bufs
    (~(put by bufs) uri.item buf)
  :_  state
  %+  weld
    (give-rpc-notification (get-diagnostics uri.item))
  [(build-file & uri.item path `dek) ~]
::
++  get-parser-diagnostics
  |=  uri=@t
  ^-  (list diagnostic:lsp-sur)
  =/  t=tape
    (zing (join "\0a" `wall`(~(got by bufs) uri)))
  =/  parse
    (lily:auto t (lsp-parser (uri-to-path:build uri)))
  ?.  ?=(%| -.parse)
    ~
  =/  loc=position:lsp-sur
    [(dec -.p.parse) +.p.parse]
  :_  ~
  [[loc loc] 1 'Syntax Error']
::
++  handle-loading
  |=  hov=text-document--hover:request:lsp-sur
  ^-  (quip card _state)
  =/  hover=card
  :*
    %pass
    /lsp
    %agent
    [our.bow %language-server]
    %poke
    %language-server-rpc-request
    !>([%text-document--hover-complete hov])
  ==
  ?~  pending.state
    ~?  >  debug  "in loading no pending: {<pending.state>}"
    [(welp ~[(wait /lsp now.bow ~s0)] loading) state(pending ~[hover])]
  =/  ped=(list card)  pending.state
  ?+    -.ped  ~?  >  debug  "in loading default: {<pending.state>}"  [~ state]
      [%pass *]
    ~?  >  debug  "in loading pass: {<pending.state>}"
    :_  state(pending ~)  done
      [%give %fact * * *]
    =-  :_  state(pending -)
    (welp ~[hover (wait /lsp now.bow ~s2)] -)
    =+  !<(=all:notification:lsp-sur q.cage.p.->.ped)
    ~?  >  debug  "in loading give: {<pending.state>} {<all>}"
    ?>  =(-.all %progress)
    =/  pro  ;;(progress:notification:lsp-sur all)
    ?:  (gte (need percentage.pro) 99)  ~
    =.  percentage.pro  `(add 1 (need percentage.pro))
    (give-rpc-notification pro)
  ==
++  handle-hover
  |=  cop=text-document--hover-complete:request:lsp-sur
  ^-  (quip card _state)
  =/  hov  hov.cop
  =|  item=text-document-item:lsp-sur
  ?.  (~(has by bufs) uri.hov)  (handle-did-open item(uri uri.hov))
  :_  state
  %^  give-rpc-response  %text-document--hover  id.hov
  =/  buf=wall
    ~|  "{<uri.hov>} not found"  (~(got by bufs) uri.hov)
  =/  txt
    (zing (join "\0a" buf))
  =/  pos  (get-pos buf row.hov col.hov)
  =/  sut  (~(gut by preludes) uri.hov -:!>(..zuse))
  =/  hon  (tape-to-hoon:auto sut pos txt)
  =/  docs=(unit @t)
    %+  biff  (find-type-mule:auto sut hon)
    |=  [id=term typ=type]
    ~?  >  debug  "looking for type: {<id>}"
    =/  found=(unit item:dprint)
      (find-item-in-type:dprint ~[id] typ)
    ~?  >  debug  "item for {<id>}: {<found>}"
    ?~  found  ~
    =;  printed=(unit @t)
      ?.  ?=(%face -.u.found)  printed
      ?~  docs.u.found  ~
      printed
    `(crip (to-tape (print-item:dprint u.found)))
  :-  ~
  =;  result
    ~?  >  debug  "output: {<result>}"  result
  ::  =-  ~?  >  debug  "exact: {<(crip -)>}"  (crip -)
  ?^  docs  u.docs
  %-  crip
  =;  exact=tape
    "```hoon\0a {exact} \0a```"
  =/  types  (=<(exact-list-tape auto) sut pos txt)
  ~?  >  debug  "types: {<types>}"
  ?:  ?=(%| -.types)  (trip (need missing-type))
  ?~  p.types  (trip (need missing-type))
  %-  zing
  %+  join  "\0a"
  =/  [=type rep=(unit tape)]  detail.u.p.types
  ~?  >  debug  "detail: {<[type rep]>}"
  ::  ?^  rep  ~[~(ram re u.rep)]
  ?^  rep  ~[u.rep]
  ?:  =(type -:!>(**))  ~[(trip (need missing-type))]
  (~(win re ~(duck easy-print type)) 0 140)
::
++  sync-buf
  |=  [buf=wall changes=(list change:lsp-sur)]
  |-  ^-  wall
  ?~  changes
    buf
  ?:  ?|(?=(~ range.i.changes) ?=(~ range-length.i.changes))
    =/  =wain  (to-wain:format text.i.changes)
    =.  buf  (turn wain trip)
    $(changes t.changes)
  =/  =tape      (zing (join "\0a" buf))
  =/  start-pos  (get-pos buf start.u.range.i.changes)
  =/  end-pos    (get-pos buf end.u.range.i.changes)
  =.  tape
    ;:  weld
      (scag start-pos tape)
      (trip text.i.changes)
      (slag end-pos tape)
    ==
  =.  buf  (to-wall tape)
  $(changes t.changes)
::
++  to-wall
  |=  =tape
  ^-  wall
  %+  roll  (flop tape)
  |=  [char=@tD =wall]
  ?~  wall
    [[char ~] ~]
  ?:  =('\0a' char)
    [~ wall]
  [[char i.wall] t.wall]
::
++  wush
  |=  [wid=@u tan=tang]
  ^-  tape
  =,  format
  (of-wall (turn (flop tan) |=(a=tank (of-wall (wash 0^wid a)))))
::
++  to-tape
  |=  sols=(list sole-effect)
  ^-  tape
  %-  zing
  =-  (turn sols -)
  |=  sef=sole-effect
    ^-  tape
    ?+    -.sef
              ~|(unsupported-effect+-.sef !!)
        %mor  (zing (turn p.sef |=(a=sole-effect ^$(sef a))))
          %txt
        :(weld "\0a" "```hoon\0a" (tape p.sef) "\0a```\0a")
        ::
        %tan  (tape (wush 160 p.sef))
        %klr  ""
    ::
        ?(%bel %clr %nex %bye)
      <+.sef>
    ==

++  get-pos
  |=  [buf=wall position]
  ^-  @ud
  ?~  buf
    0
  ?:  =(0 row)
    col
  %+  add  +((lent i.buf))  ::  +1 because newline
  $(row (dec row), buf t.buf)
::
++  safe-sub
  |=  [a=@ b=@]
  ?:  (gth b a)
    0
  (sub a b)
::
++  missing-type  ^-  (unit @t)  [~ 'No type found']
++  loading  ^-  (list card)  (give-rpc-notification [%progress 'Loading...' 'begin' ~ `0])
++  done  ^-  (list card)  (give-rpc-notification [%progress 'Done.' 'end' ~ `100])
++  wait
  |=  [=path now=@da time=@dr]
  ^-  card
  [%pass [%timer path] %arvo %b %wait (add now time)]
::    +cite
::
::  write file to desk.
++  cite 
  |=  [=desk =path data=cage]
  =*  bek  byk.bow
  ~&  >>  "saving to {<desk>} at {<path>}"
  ~|  "failed write to {<desk>}"
  =-  [%pass /lsp/write %arvo %c %info -]~
  =/  fath=^path  (weld /(scot %p our.bow)/[desk]/(scot %da now.bow) path)
  ~&  >>  "saved to {<desk>} at {<path>}"
  (foal:space:userlib fath data)
::    +hite
::
::  write text as hoon to desk.
++  hite
      |=  [=desk =path txt=@t]
      =/  =mark  (rear path)
      =/  =type  [%atom %t ~]
      =-  (cite desk path -)
      [mark [type ?:(=(%hoon mark) txt (need (de-json:html txt)))]]
--
