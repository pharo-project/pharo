merging
mergeMap
    | map |
    map := super mergeMap.
    map at: #'versionString' put: versionString.
    map at: #'blessing' put: blessing.
    map at: #'description' put: description.
    map at: #'author' put: author.
    map at: #'timestamp' put: timestamp.
    map at: #'preLoadDoIt' put: preLoadDoIt.
    map at: #'postLoadDoIt' put: postLoadDoIt.
    map at: #'packageList' put: self packages.
    ^ map