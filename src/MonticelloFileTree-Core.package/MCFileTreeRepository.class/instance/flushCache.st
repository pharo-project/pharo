private
flushCache
  "force properties to be reread ... if the directory exists, otherwise let nature
   take it's course"

  super flushCache.
  directory
    ifNotNil: [ 
      (MCFileTreeFileUtils current directoryExists: directory)
        ifTrue: [ 
          repositoryProperties := nil.
          self repositoryProperties ] ]