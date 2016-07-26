private
defaultCacheDirectory
  | defaultDirectory cacheDirectory |
  defaultDirectory := MetacelloPlatform current defaultDirectory.
  cacheDirectory := MetacelloPlatform current
    directoryFromPath: self basicDescription , '-cache'
    relativeTo: defaultDirectory.
  MetacelloPlatform current ensureDirectoryExists: cacheDirectory.
  ^ cacheDirectory