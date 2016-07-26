private
parseProjectVersionField: parseBlock desc: desc versionDelim: versionDelim
  "Issue #234: have to allow for commitish containing slashes"

  | strm done escaped repoDelim |
  strm := WriteStream on: String new.
  repoDelim := versionDelim + 1.
  escaped := done := false.
  [ done ]
    whileFalse: [ 
      | char |
      repoDelim > desc size
        ifTrue: [ done := true ]
        ifFalse: [ 
          char := desc at: repoDelim.
          char == $\
            ifTrue: [ 
              escaped
                ifTrue: [ 
                  "$\ not legal in branch name ... literally ignored"
                  escaped := false ]
                ifFalse: [ escaped := true ] ]
            ifFalse: [ 
              char == $/
                ifTrue: [ 
                  escaped
                    ifFalse: [ done := true ] ].
              done
                ifFalse: [ strm nextPut: char ].
              escaped := false ].
          repoDelim := repoDelim + 1 ] ].
  repoDelim := repoDelim - 1.
  parseBlock
    value: strm contents
    value: (desc copyFrom: repoDelim + 1 to: desc size)