printing
printOn: aStream

	(#(String) includes: self arrayClass name) 
		ifTrue: [^self storeOn: aStream].
	^super printOn: aStream
