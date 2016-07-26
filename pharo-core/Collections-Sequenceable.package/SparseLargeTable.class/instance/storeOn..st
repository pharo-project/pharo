printing
storeOn: aStream

	| x |
	(#(String) includes: self arrayClass name) ifTrue: 
		[aStream nextPut: $'.
		1 to: self size do:
			[:i |
			aStream nextPut: (x := self at: i).
			x == $' ifTrue: [aStream nextPut: x]].
		aStream nextPutAll: ''' asLargeArrayChunkSize: '.
		aStream nextPutAll: self chunkSize asString.
		^self].
	^super storeOn: aStream
