emitting code
offsetWriteFieldAt: offsetVariableName with: valueName
	^ String streamContents: [ :stream | 
		stream << 'handle unsignedLongAt: ' << offsetVariableName << ' put: anObject thunk address' ]