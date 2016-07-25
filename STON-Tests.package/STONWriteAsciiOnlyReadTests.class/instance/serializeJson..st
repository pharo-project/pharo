private
serializeJson: anObject
	^ String streamContents: [ :stream |
		STON jsonWriter 
			on: stream; 
			asciiOnly: true;
			nextPut: anObject ]