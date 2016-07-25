parsing-internal
parseNumberInteger
    | number |
    number := 0.
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next charCode - 48) ].
    ^ number