emitting code
writeFieldAt: byteOffset with: valueName
	^ self externalTypeWithArity 
		writeFieldAt: byteOffset 
		with: valueName