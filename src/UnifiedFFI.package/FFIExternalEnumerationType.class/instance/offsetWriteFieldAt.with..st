emitting code
offsetWriteFieldAt: offsetVariableName with: valueName
	^ String streamContents: [ :stream | 
		stream << 'handle ' << self externalType atomicSelector << ' ' << offsetVariableName << ' put: anObject value' ]