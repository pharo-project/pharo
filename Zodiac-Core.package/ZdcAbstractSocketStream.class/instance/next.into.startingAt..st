stream in
next: requestedCount into: collection startingAt: offset
	"Read requestedCount elements into collection starting at offset,
	returning a copy if less elements are available"
	
	| read |
	read := self 
		readInto: collection 
		startingAt: offset 
		count: requestedCount.
	^ read = requestedCount 
		ifTrue: [ collection ]
		ifFalse: [ collection copyFrom: 1 to: offset + read - 1 ]     