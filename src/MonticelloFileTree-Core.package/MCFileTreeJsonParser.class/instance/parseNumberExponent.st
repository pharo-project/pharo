parsing-internal
parseNumberExponent
    | number negated |
    number := 0.
    negated := stream peek = $-.
    (negated or: [ stream peek = $+ ])
        ifTrue: [ stream next ].
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next charCode - 48) ].
    negated
        ifTrue: [ number := number negated ].
    ^ 10 raisedTo: number