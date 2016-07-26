instance creation
signal: aString streamPosition: streamPosition 
	^ self new
		streamPosition: streamPosition;
		signal: aString;
		yourself