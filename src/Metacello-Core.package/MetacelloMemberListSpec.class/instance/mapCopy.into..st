private
mapCopy: aMemberSpec into: map
    | spec |
    spec := map at: aMemberSpec sourceName ifAbsent: [  ].
    spec == nil
        ifTrue: [ ^ self error: 'Source spec named ' , aMemberSpec sourceName printString , ' not found' ]
        ifFalse: [ 
            spec aboutToCopy.
            map at: aMemberSpec name put: (spec copy mergeSpec: aMemberSpec spec copy) ]