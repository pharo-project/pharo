emitting code
writeFieldAt: byteOffset with: valueName
	^ String streamContents: [ :stream | 
		stream << 'handle ' << self externalType atomicSelector << ' ' << byteOffset asString << ' put: anObject value' ]