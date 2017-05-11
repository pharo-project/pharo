accessing
repositoryProperties
  repositoryProperties
    ifNil: [ 
      repositoryProperties := Dictionary new.
      (self fileUtils directoryExists: directory)
        ifFalse: [ 
          self
            error:
              'filetree:// repository '
                ,
                  (self fileUtils directoryPathString: self directory) printString
                , ' does not exist.' ].
      (self directory entries
        detect: [ :entry | entry name = '.filetree' ]
        ifNone: [  ])
        ifNil: [ 
          repositoryProperties := self defaultRepositoryProperties.
          self writeRepositoryProperties ]
        ifNotNil: [ :configEntry | 
          configEntry
            readStreamDo: [ :fileStream | repositoryProperties := STON fromStream: fileStream ] ] ].
  ^ repositoryProperties