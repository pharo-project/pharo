defaults
defaultVersionString
    DefaultVersionString ifNil: [ DefaultVersionString := #'stable' ].
    ^ DefaultVersionString