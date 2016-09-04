accessing
nextPut: anObject
	"Ensure that the limit is not exceeded"

	position >= limit
		ifTrue: [ limitBlock value ]
		ifFalse: [ super nextPut: anObject ].
	^ anObject