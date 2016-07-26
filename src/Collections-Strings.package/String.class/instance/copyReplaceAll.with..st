copying
copyReplaceAll: oldSubstring with: newSubstring
    "Answer a copy of the receiver in which all occurrences of oldSubstring have been replaced by newSubstring"

    | idx |        
    self = oldSubstring ifTrue: [ ^ newSubstring copy ].
    oldSubstring isEmpty ifTrue: [ ^ self copy ].
    idx := 1.
    ^ self class new: self size streamContents: [ :stream | | foundIdx |
        [ (foundIdx := self findString: oldSubstring startingAt: idx) isZero ] whileFalse: [ 
            stream 
                next: (foundIdx - idx) putAll: self startingAt: idx;
                nextPutAll: newSubstring.
            idx := foundIdx + oldSubstring size ].
        idx <= self size ifTrue: [ 
            stream next: (self size - idx + 1) putAll: self startingAt: idx ] ]