private
resetCacheDirectoryIfInvalid
  "use class var to survive upgrade when MCGitHubRepository moved to subclass of MCGitBasedRepository"

  "Reset if invalid"

  CacheDirectory notNil
    and: [ 
      (MCFileTreeFileUtils current directoryExists: CacheDirectory)
        ifFalse: [ CacheDirectory := nil ] ]