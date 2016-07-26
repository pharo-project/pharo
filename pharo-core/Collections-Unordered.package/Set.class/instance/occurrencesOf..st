testing
occurrencesOf: anObject 
	^ (self includes: anObject) ifTrue: [1] ifFalse: [0]