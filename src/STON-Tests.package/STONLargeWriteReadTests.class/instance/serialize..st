private
serialize: anObject
	^ String streamContents: [ :stream |
		STON writer 
			on: stream; 
			optimizeForLargeStructures; 
			nextPut: anObject ]