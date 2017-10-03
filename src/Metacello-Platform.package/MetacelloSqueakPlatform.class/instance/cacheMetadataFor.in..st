private
cacheMetadataFor: aWebResponse in: aDictionary

  "Copy interesting"
  #( 'ETag' 'Last-Modified' 'X-RateLimit-Remaining' ) do: [:key |
    (aWebResponse headerAt: key) ifNotEmpty: [:value| aDictionary at: key put: value]].