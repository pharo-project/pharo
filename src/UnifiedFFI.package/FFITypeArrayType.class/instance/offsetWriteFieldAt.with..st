emitting code
offsetWriteFieldAt: offsetVariableName with: valueName
	self flag: #todo. "This is not working in all cases for 32/64 bits (
	externalTypeSize could be different)"
	^ String streamContents:[ :stream|
		stream 
			nextPutAll:'handle replaceFrom: ';
			nextPutAll: offsetVariableName;
			nextPutAll: ' to: ';
			nextPutAll: (offsetVariableName, ' + ', (self externalTypeSize - 1) asString);
			nextPutAll: ' with: anObject getHandle startingAt: 1' ].