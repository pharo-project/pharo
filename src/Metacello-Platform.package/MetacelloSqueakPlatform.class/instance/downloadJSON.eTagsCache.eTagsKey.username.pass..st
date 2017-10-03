github/bitbucket support
downloadJSON: url eTagsCache: eTagsCache eTagsKey: eTagsKey username: username pass: pass
  "return result of parsing JSON downloaded from url. username:pass may be nil, but calls will be subject to severe rate limits."

  "To minimize GitHub rate limiting problems, we are caching the ETags for the tag request: https://developer.github.com/v3/#conditional-requests"

  | info response |
  eTagsCache ifNil: [^ self self downloadJSON: url username: username pass: pass].
  Transcript cr.
  info := eTagsCache at: eTagsKey ifAbsentPut: [Dictionary new].
  response := self httpGet: url username: username pass: pass do: [:req |
    info
      at: 'ETag'
      ifPresent: [:tag | req headerAt: 'If-None-Match' put: tag]
      ifAbsent: [req headerAt: 'If-Modified-Since' put:  'Mon, 23 Apr 2012 14:17:47 GMT']].

  response isSuccess | response isRedirect
    ifFalse: [ Transcript cr; show: 'downloadJSON:eTagsCache:eTagsKey:username:pass: ', response printString ].
 
  response code = 304 ifTrue: [" Not modified"
    ^ info
      at: 'jsonObject'
      ifPresent: [:obj | self cacheMetadataFor: response in: info. obj]
      ifAbsent: [self downloadJSON: url username: username pass: pass]].
  
  ^ response isSuccess
    ifFalse: [MCFileTreeJsonParser parse: response content]
    ifTrue: [self cacheJSONFor: response in: info]

  