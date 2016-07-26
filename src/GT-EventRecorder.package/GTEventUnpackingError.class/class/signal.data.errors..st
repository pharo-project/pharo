instance creation
signal: aString data: aByteArray errors: aCollection 
	^ self new
			data: aByteArray;
			errors: aCollection;
			signal: aString;
			yourself