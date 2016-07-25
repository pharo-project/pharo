accessing
eTagsCache
  "To avoid GitHub rate limiting problems, we are caching the ETags for the tag request: https://developer.github.com/v3/#conditional-requests"

  ETagsCache ifNil: [ ETagsCache := Dictionary new ].
  ^ ETagsCache