emitting code
writeFieldAt: byteOffset with: valueName
	^ String streamContents: [ :stream | 
		stream << 'handle pointerAt: ' << byteOffset asString << ' put: (ExternalAddress fromAddress: anObject thunk address)' ]