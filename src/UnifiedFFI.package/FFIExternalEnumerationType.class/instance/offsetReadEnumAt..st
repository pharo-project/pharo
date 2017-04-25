emitting code
offsetReadEnumAt: offsetVariableName
	^ String streamContents:[ :stream|
		stream 
			nextPutAll:'^';
			print: self objectClass;
			nextPutAll:' fromInteger: (handle ', self externalType atomicSelector,' ';
			nextPutAll: offsetVariableName;
			nextPutAll:')' ].
