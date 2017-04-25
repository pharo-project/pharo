emitting code
offsetWriteFieldAt: offsetVariableName with: valueName
	^ String streamContents: [ :stream | 
		stream << 'handle pointerAt: ' << offsetVariableName << ' put: (ExternalAddress fromAddress: anObject thunk address)' ]