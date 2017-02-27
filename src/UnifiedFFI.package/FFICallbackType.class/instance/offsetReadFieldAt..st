emitting code
offsetReadFieldAt: offsetVariableName
	^ String streamContents: [ :stream | 
		stream << '^' << self objectClass name << ' forAddress: (handle unsignedLongAt: ' << offsetVariableName asString << ')' ]