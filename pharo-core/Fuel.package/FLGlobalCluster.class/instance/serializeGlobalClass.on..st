serialize/materialize
serializeGlobalClass: aClass on: anEncoder
	
	aClass isObsolete ifTrue: [ FLObsolete signalWithName: aClass name ].
	anEncoder encodeString: aClass name 