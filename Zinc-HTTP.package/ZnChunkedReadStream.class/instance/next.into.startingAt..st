accessing
next: requestedCount into: collection startingAt: offset
	"Read requestedCount elements into collection starting at offset,
	returning a copy if less elements are available"
	
	| readCount |
	readCount := self 
		readInto: collection 
		startingAt: offset 
		count: requestedCount.
	^ requestedCount = readCount
		ifTrue: [ collection ]
		ifFalse: [ collection copyFrom: 1 to: offset + readCount - 1 ]