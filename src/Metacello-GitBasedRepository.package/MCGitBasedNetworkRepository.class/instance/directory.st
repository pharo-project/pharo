accessing
directory
  directory
    ifNil: [ 
      directory := self class
        projectDirectoryFrom: self projectPath
        version: self projectVersion.
      self repoPath isEmpty
        ifFalse: [ 
          directory := MetacelloPlatform current
            directoryFromPath: self repoPath
            relativeTo: directory ] ]
    ifNotNil: [ 
      (MCFileTreeFileUtils current directoryExists: directory)
        ifFalse: [ 
          self flushCache.
          ^ self directory ] ].
  ^ directory