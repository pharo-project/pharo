printing
print: components prefix: prefixChar on: aStream
    | beforeFirst |
    beforeFirst := true.
    components
        do: [ :component | 
            beforeFirst
                ifTrue: [ 
                    beforeFirst := false.
                    prefixChar ifNotNil: [ aStream nextPut: prefixChar ] ]
                ifFalse: [ aStream nextPut: $. ].
            aStream nextPutAll: component asString ]