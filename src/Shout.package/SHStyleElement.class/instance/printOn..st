printing
printOn: aStream
	super printOn: aStream.
	color isNil
		ifFalse: [ 
			aStream
				nextPutAll: ' color: ';
				nextPutAll: color printString ].
	emphasis isNil
		ifFalse: [ 
			aStream
				nextPutAll: ' emphasis: ';
				nextPutAll: emphasis printString ].
	tokens isNil
		ifFalse: [ 
			aStream
				nextPutAll: ' tokens: ';
				nextPutAll: tokens printString ]