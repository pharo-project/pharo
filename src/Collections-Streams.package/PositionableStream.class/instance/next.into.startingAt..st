accessing
next: requestedCount into: aCollection startingAt: startIndex
	"Read requestedCount objects into the given collection. 
	Return aCollection or a partial copy if less elements have been read."

	| readCount |
	readCount := self readInto: aCollection startingAt: startIndex count: requestedCount.
	^ readCount = requestedCount
		ifTrue: [ ^ aCollection ]
		ifFalse: [ ^ aCollection copyFrom: 1 to: startIndex + readCount - 1 ]