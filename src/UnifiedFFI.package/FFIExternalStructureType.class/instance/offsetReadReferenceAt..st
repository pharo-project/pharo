emitting code
offsetReadReferenceAt: offsetVariableName
	^ String streamContents:[ :stream|
		stream 
			nextPutAll:'^';
			print: self objectClass;
			nextPutAll:' fromHandle: (handle referenceStructAt: ';
			nextPutAll: offsetVariableName;
			nextPutAll:' length: ';
			print: self externalTypeSize;
			nextPutAll:')' ].
