data flow instructions
pushNewArrayOfSize: numElements
	"tempVector"

	| tempVect |
	tempVect := self createNTemps: numElements.
	currentSequence addTemporariesNamed: (tempVect collect: [ :e | e name ]).
	simulatedStack addLast: tempVect