github/bitbucket support
downloadJSON: url username: username pass: pass
  "return result of parsing JSON downloaded from url. username:pass may be nil, but calls will be subject to severe rate limits."

  | client json |
  client := ZnClient new
    url: url;
    yourself.
  username ifNotNil: [ client username: username password: pass ].
  client get.
  json := client contents.
  ^ STON fromString: json
