private
serialize: anObject
	^ String streamContents: [ :stream |
		STON writer 
			on: stream; 
			asciiOnly: true;
			nextPut: anObject ]