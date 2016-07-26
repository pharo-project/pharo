copying
postCopy
    super postCopy.
    mutable := nil.
    loader ~~ nil
        ifTrue: [ self loader: loader ].