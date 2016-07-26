menu messages
selectedSelector
    "Try to make a selector out of the current text selection"

    | node |
    node := RBParser parseFaultyExpression: self selection string.
    node isMessage
        ifTrue: [ ^ node selector ].
    node isSequence "sequence of error nodes, we take the first one"
        ifTrue: [ ^ node  statements first value asSymbol].
    ^ node name