merging
mergeMap
    | map |
    map := super mergeMap.
    map at: #'repositories' put: self repositories.
    ^ map