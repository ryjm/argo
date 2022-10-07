/-  lsp=language-server
|%
::
++  util
  |%
  ++  get-json-string
    |=  [jon=(map @t json) key=@t]
    ^-  (unit cord)
    =/  cord-jon=(unit json)
      (~(get by jon) key)
    ?~  cord-jon
      ~
    ?>  ?=([%s *] u.cord-jon)
    `p.u.cord-jon
  --
::
::
++  dejs
  =,  dejs:format
  |%
  ++  request
    |=  jon=json
    ?>  ?=([%o *] jon)
    =/  method=cord
      %-  method
      (trip (need (get-json-string:util p.jon 'method')))
    =/  id=cord
      (need (get-json-string:util p.jon 'id'))
    =/  params=json
      (~(got by p.jon) 'params')
    ^-  all:request:lsp
    |^
      ?+  method  [%unknown jon]
        %text-document--hover       (text-document--hover params id)
        %text-document--completion  (text-document--completion params id)
      ==
      ::
      ++  text-document--hover
        |=  [params=json id=cord]
        ^-  text-document--hover:request:lsp
        :+  %text-document--hover
          id
        %.  params
        %:  ot
          position+position
          'textDocument'^text-document-id
          ~
        ==
      ::
      ++  text-document--completion
        |=  [params=json id=cord]
        :+  %text-document--completion  id
        %.  params
        %:  ot
          position+position
          'textDocument'^text-document-id
          ~
        ==
      --
  ::
  ++  notification
    |=  jon=json
    ?>  ?=([%o *] jon)
    =/  method=cord
      %-  method
      (trip (need (get-json-string:util p.jon 'method')))
    =/  params=json
      (~(got by p.jon) 'params')
    ^-  all:notification:lsp
    |^
      ?+  method  [%unknown jon]
          %text-document--did-change
        (text-document--did-change params)
          %progress
        (progres params)
          %text-document--did-open
        (text-document--did-open params)
          %text-document--did-save
        (text-document--did-save params)
          %text-document--did-close
        (text-document--did-close params)
      ==
      ::
      ++  text-document--did-save
        |=  jon=json
        ^-  text-document--did-save:notification:lsp
        ?>  ?=([%o *] jon)
        =/  doc-id=json
          (~(got by p.jon) 'textDocument')
        :-  %text-document--did-save
        (text-document-id doc-id)

      ::
      ++  text-document--did-close
        |=  jon=json
        ^-  text-document--did-close:notification:lsp
        ?>  ?=([%o *] jon)
        =/  doc-id
          (~(got by p.jon) 'textDocument')
        :-  %text-document--did-close
        (text-document-id doc-id)
      ::
      ++  text-document--did-change
        |=  jon=json
        ^-  text-document--did-change:notification:lsp
        :-  %text-document--did-change
        %.  jon
        %:  ot
          'textDocument'^text-document-id
          'contentChanges'^text-document-changes
          ~
        ==
      ::
      ++  text-document--did-open
        |=  jon=json
        ^-  text-document--did-open:notification:lsp
        ?>  ?=([%o *] jon)
        :-  %text-document--did-open
        (text-document-item (~(got by p.jon) 'textDocument'))
      ::
      ++  progres
        |=  jon=json
        ^-  progress:notification:lsp
        ?>  ?=([%o *] jon)
        :-  %progress
        %.  (~(got by p.jon) 'value')
        %:  ot
          title+so
          kind+so
          message+(mu so)
          percentage+(mu ni)
          ~
        ==
    --
    ::  Utilities
    ::
  ++  text-document-item
    |=  jon=json
    ^-  text-document-item:lsp
    %.  jon
    %:  ot
      uri+so
      version+(mu ni)
      text+so
      ~
    ==
  ::
  ++  text-document-id
    |=  jon=json
    ^-  versioned-doc-id:lsp
    %.  jon
    %:  ou
      uri+(un so)
      version+(uf ~ (pe ~ ni))
      ~
    ==
  ::
  ++  text-document-changes
    %-  ar
    %:  ou
        range+(uf ~ (pe ~ range))
        'rangeLength'^(uf ~ (pe ~ ni))
        text+(un so)
        ~
    ==
  ::
  ++  method
    |=  =tape
    ^-  cord
    %-  crip  %-  zing
    %+  join  "--"
    ^-  (list ^tape)
    %+  turn
      ^-  (list (list ^tape))
      %+  scan
        tape
      %+  more
        fas
      ;~  plug
        (star ;~(pose buc low))
        (star ;~(plug (cook |=(a=@ (add a 32)) hig) (star low)))
      ==
    |=  words=(list ^tape)
    ^-  ^tape
    (zing (join "-" words))
  ::
  ++  range
    %:  ot
      start+position
      end+position
      ~
    ==
  ::
  ++  position
    %:  ot
        line+ni
        character+ni
        ~
    ==
  --
::
++  enjs
  =,  enjs:format
  |%
  ++  progress
    |=  pog=progress:notification:lsp
    ^-  json
    %+  frond  'value'
    %-  pairs
    :~
      title+s+title.pog
      kind+s+kind.pog
      ?~  message.pog  message+~  message+s+u.message.pog
      ?~  percentage.pog  percentage+~  percentage+(numb u.percentage.pog)
    ==
  ++  text-document--publish-diagnostics
    |=  pub=text-document--publish-diagnostics:notification:lsp
    ^-  json
    ~&  pub+pub
    %:  pairs
      uri+s+uri.pub
      diagnostics+a+(turn diagnostics.pub diagnostic)
      ~
    ==
  ++  notification
    |=  notification=all:notification:lsp
    ^-  json
    =/  params=json
      ?+  -.notification  !!
            %text-document--publish-diagnostics
          (text-document--publish-diagnostics notification)
            %progress
          (progress notification)

        ==
    ~!  -.notification
    =/  method=cord  (crip (unparse-method -.notification))
    %:  pairs
      method+s+method
      params+params
      ~
    ==
  ::
  ++  response
    |=  res=all:response:lsp
    ^-  json
    |^
      ?-  -.res
        %text-document--hover       (text-document--hover res)
        %text-document--completion  (text-document--completion res)
      ==
      ::
      ++  wrap-in-id
        |=  [id=cord res=json]
        %:  pairs
          id+s+id
          result+res
          ~
        ==
      ++  text-document--hover
        |=  hov=text-document--hover:response:lsp
        %+  wrap-in-id  id.hov
        %+  frond  'contents'
        ?~  contents.hov
          ~
        s+u.contents.hov
      ::
      ++  text-document--completion
        |=  com=text-document--completion:response:lsp
        %+  wrap-in-id   id.com
        [%a (turn completion.com completion-item)]
      --
  ++  unparse-method
    |=  =cord
    ^-  ^tape
    %+  rash  cord
    %+  cook
      |=  l=(list ^tape)
      %-  zing  (join "/" l)
    %+  more  (jest '--')
    %+  cook
      |=  tapes=(list ^tape)
      =/  pre=?  =((lent tapes) 1)
      =?  tapes  pre
        (welp ~["$/"] tapes)
      ^-  ^tape
      ?~  tapes  ~
      %-  zing
      :-  i.tapes
      %+  turn  t.tapes
      |=  t=^tape
      ^-  ^tape
      ?~  t  ~
      [`@tD`?:(pre i.t (sub i.t 32)) t.t]
    %+  more
      ;~(less (jest '--') hep)
    (star ;~(pose buc fas alf))
  ::
  ++  completion-item
    |=  com=completion-item:lsp
    ^-  json
    %:  pairs
      label+s+label.com
      detail+s+detail.com
      kind+(numb kind.com)
      'documentation'^s+doc.com
      'insertText'^s+insert-text.com
      'insertTextFormat'^(numb insert-text-format.com)
      ~
    ==
  ::
  ++  position
    |=  =position:lsp
    ^-  json
    %:  pairs
      line+(numb row.position)
      character+(numb col.position)
      ~
    ==
  ::
  ++  range
    |=  =range:lsp
    ^-  json
    %:  pairs
      start+(position start.range)
      end+(position end.range)
      ~
    ==
  ::
  ++  diagnostic
    |=  diag=diagnostic:lsp
    ^-  json
    %:  pairs
      range+(range range.diag)
      severity+(numb severity.diag)
      message+s+message.diag
      ~
    ==
  ::
  --
--
