merging
mergeMap
    | map |
    map := super mergeMap.
    map at: #'description' put: description.
    map at: #'type' put: self type.
    map at: #'username' put: username.
    map at: #'password' put: password.
    ^ map