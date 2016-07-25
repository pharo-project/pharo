initialize-release
flushCache
  "the directory acts like a cache since we download the directory from a git-based repository (github, bitbucket, etc.)"

  [ super flushCache ]
    on: Error
    do: [ :ex | 
      Transcript
        cr;
        show:
            'Error for: ' , self description printString , ' during flushCache: '
                , ex description printString ].
  self class flushDownloadCache.
  directory := nil