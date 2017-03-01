emitting code
readFieldAt: byteOffset
	^ String streamContents: [ :stream | 
		stream << '^' << self objectClass name << ' forAddress: ((handle pointerAt: ' << byteOffset asString << ') asInteger)' ]