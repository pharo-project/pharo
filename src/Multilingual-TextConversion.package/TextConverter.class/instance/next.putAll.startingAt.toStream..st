conversion
next: anInteger putAll: aString startingAt: startIndex toStream: aStream
        "Handle fast conversion if ByteString"
        
        | lastIndex nextIndex |
        aString class == ByteString ifFalse: [
                startIndex to: startIndex + anInteger - 1 do: [ :index |
                        self nextPut: (aString at: index) toStream: aStream ].
                ^aString ].
        aStream isBinary ifTrue: [
                aStream basicNext: anInteger putAll: aString startingAt: startIndex.
                ^aString ].
        
        ^self next: anInteger putByteString: aString startingAt: startIndex toStream: aStream