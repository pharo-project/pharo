private
cacheDirectory
  self resetCacheDirectoryIfInvalid.
  repoCacheDirectory ifNil: [ repoCacheDirectory := self defaultCacheDirectory ].
  ^ repoCacheDirectory