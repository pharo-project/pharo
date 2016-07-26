private
compareEqualTo: myComponents other: otherComponents
    | mySize |
    mySize := myComponents size.
    mySize = otherComponents size
        ifFalse: [ ^ false ].
    1 to: mySize do: [ :i | 
        (myComponents at: i) = (otherComponents at: i)
            ifFalse: [ ^ false ] ].
    ^ true