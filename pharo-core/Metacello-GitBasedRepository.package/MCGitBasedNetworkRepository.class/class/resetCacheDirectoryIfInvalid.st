private
resetCacheDirectoryIfInvalid
  "Reset if invalid"

  repoCacheDirectory notNil
    and: [ 
      (MCFileTreeFileUtils current directoryExists: repoCacheDirectory)
        ifFalse: [ repoCacheDirectory := nil ] ]