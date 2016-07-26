comparing
match: aVersionPattern
    "Answer whether the version number of the receiver matches the given pattern string.

	 A Metacello version number is made up of version sequences delimited by the characters $. and $-.
	 The $. introduces a numeric version sequence and $- introduces an alphanumeric version sequence.
	 
	 A version pattern is made up of version pattern match sequences. also delimited by the characters $. 
	 and $-.. Each pattern match sequence is tested against the corresponding version sequence of the 
	 receiver, using the 'standard' pattern matching rules. All sequences must answer true for a match.
	
	 The special pattern sequence '?' is a match for the corresponding version sequence and all subsequent 
	 version sequences. '?' as the version pattern matches all versions. No more version pattern 
	 sequences are permitted once the '?' sequence is used. If used, it is the last version pattern
	 sequence. "

    | patternVersion mySize patternSize components |
    patternVersion := (self class fromString: aVersionPattern forPattern: true) versionComponents.
    components := self versionComponents.
    mySize := components size.
    patternSize := patternVersion size.
    mySize = patternSize
        ifFalse: [ 
            mySize < patternSize
                ifTrue: [ ^ false ].
            (patternVersion at: patternSize) ~= '?'
                ifTrue: [ ^ false ].
            mySize := patternSize ].
    1 to: mySize do: [ :i | 
        | pattern |
        pattern := (patternVersion at: i) asString.
        pattern = '?'
            ifTrue: [ 
                i = mySize
                    ifFalse: [ ^ self error: 'Invalid version match pattern: ' , aVersionPattern printString ] ]
            ifFalse: [ 
                (pattern match: (components at: i) asString)
                    ifFalse: [ ^ false ] ] ].
    ^ true