emitting code
writeFieldAt: byteOffset with: valueName
	^ String streamContents: [ :stream | 
		stream << 'handle unsignedLongAt: ' << byteOffset asString << ' put: anObject thunk address' ]