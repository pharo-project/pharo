stream in
next: requestedCount into: collection startingAt: startIndex
	"Read requestedCount objects into the given collection. 
	Return collection or a partial copy if less elements have been read."

	| readCount |
	readCount := self readInto: collection startingAt: startIndex count: requestedCount.
	^ readCount = requestedCount
		ifTrue: [ ^ collection ]
		ifFalse: [ ^ collection copyFrom: 1 to: startIndex + readCount - 1 ]