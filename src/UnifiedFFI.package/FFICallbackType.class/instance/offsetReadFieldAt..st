emitting code
offsetReadFieldAt: offsetVariableName
	^ String streamContents: [ :stream | 
		stream << '^' << self objectClass name << ' forAddress: ((handle pointerAt: ' << offsetVariableName asString << ') asInteger)' ]