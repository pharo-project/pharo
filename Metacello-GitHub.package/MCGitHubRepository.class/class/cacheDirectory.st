private
cacheDirectory
  "use class var to survive upgrade when MCGitHubRepository moved to subclass of MCGitBasedRepository"

  self resetCacheDirectoryIfInvalid.
  CacheDirectory ifNil: [ CacheDirectory := self defaultCacheDirectory ].
  ^ CacheDirectory