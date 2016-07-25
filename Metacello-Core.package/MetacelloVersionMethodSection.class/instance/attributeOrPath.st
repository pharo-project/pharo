accessing
attributeOrPath
    ^ self parent == nil
        ifTrue: [ 
            self attributes size == 1
                ifTrue: [ self attributes first ]
                ifFalse: [ self attributes ] ]
        ifFalse: [ self attributePath ]