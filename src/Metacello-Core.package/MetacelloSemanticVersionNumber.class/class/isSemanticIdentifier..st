private
isSemanticIdentifier: aString
    "whether the receiver is composed entirely of alphanumerics"

    aString
        do: [ :c | 
            c isAlphaNumeric
                ifFalse: [ 
                    c = $-
                        ifFalse: [ ^ false ] ] ].
    ^ true