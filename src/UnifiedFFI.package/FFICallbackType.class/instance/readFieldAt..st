emitting code
readFieldAt: byteOffset
	self error: 'TODO'.
	^ String streamContents: [ :stream | 
		stream << '^' << self objectClass name << ' forAddress: (handle unsignedLongAt: ' << byteOffset asString << ')' ]