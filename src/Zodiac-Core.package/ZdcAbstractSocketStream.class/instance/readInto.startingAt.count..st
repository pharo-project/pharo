stream in
readInto: collection startingAt: offset count: requestedCount
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read, there could be less elements available.
	This is an inefficient abstract implementation, reading bytes one by one."
	
	0 to: requestedCount - 1 do: [ :count | | object |
		(object := self next) ifNil: [ ^ count ].  
		collection at: offset + count put: object ].
	^ requestedCount