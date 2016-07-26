private
compareLessThan: myComponents other: otherComponents version: version
    | mySize aSize commonSize count more |
    mySize := myComponents size.
    aSize := otherComponents size.
    commonSize := mySize min: aSize.
    count := 0.
    more := true.
    [ more and: [ count < commonSize ] ]
        whileTrue: [ 
            (myComponents at: count + 1) = (otherComponents at: count + 1)
                ifTrue: [ count := count + 1 ]
                ifFalse: [ more := false ] ].
    count < commonSize
        ifTrue: [ ^ (myComponents at: count + 1) metacelloSemanticVersionComponentLessThan: (otherComponents at: count + 1) ].
    mySize < aSize
        ifTrue: [ 
            mySize = 0
                ifTrue: [ 
                    #'preRelease' == version
                        ifTrue: [ ^ false ].
                    ^ true ].
            (myComponents at: commonSize) = (otherComponents at: commonSize)
                ifFalse: [ ^ true ].
            ^ true ]
        ifFalse: [ 
            mySize = aSize
                ifTrue: [ ^ false ].
            aSize = 0
                ifTrue: [ 
                    #'build' == version
                        ifTrue: [ ^ false ].
                    ^ true ].
            (myComponents at: commonSize) = (otherComponents at: commonSize)
                ifFalse: [ ^ false ].
            ^ true ]