github/bitbucket support
downloadJSON: url username: username pass: pass
  "return result of parsing JSON downloaded from url. username:pass may be nil, but calls will be subject to severe rate limits."

  | response |
  response := self httpGet: url username: username pass: pass do: [:req | ].
  response isSuccess
    ifFalse: [ Transcript cr; show: 'downloadJSON:username:pass: ', response printString ].

  ^ MCFileTreeJsonParser parse: response content