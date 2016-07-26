accessing
attributePath
    | path |
    path := MetacelloMethodSectionPath with: self attributes.
    ^ self parent == nil
        ifTrue: [ path ]
        ifFalse: [ self parent attributePath , path ]