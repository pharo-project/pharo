accessing
priority: anInteger
	"Set the receiver's priority to anInteger."

	(anInteger between: Processor lowestPriority and: Processor highestPriority)
		ifTrue: [ priority := anInteger ]
		ifFalse: [ self error: 'Invalid priority: ' , anInteger printString ]