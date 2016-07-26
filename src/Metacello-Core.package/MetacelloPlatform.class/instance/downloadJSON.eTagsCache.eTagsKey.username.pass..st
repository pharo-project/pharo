github/bitbucket support
downloadJSON: url eTagsCache: eTagsCache eTagsKey: eTagsKey username: username pass: pass
  "return result of parsing JSON downloaded from url. username:pass may be nil, but calls will be subject to severe rate limits."

  "To minimize GitHub rate limiting problems, we are caching the ETags for the tag request: https://developer.github.com/v3/#conditional-requests"

  ^ self downloadJSON: url username: username pass: pass