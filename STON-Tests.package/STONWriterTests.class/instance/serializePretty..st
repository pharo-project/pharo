private
serializePretty: anObject
	^ String streamContents: [ :stream |
		STON writer 
			on: stream; 
			prettyPrint: true;
			nextPut: anObject ]