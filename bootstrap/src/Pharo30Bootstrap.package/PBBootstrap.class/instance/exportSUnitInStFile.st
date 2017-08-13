preparation
exportSUnitInStFile

	| mcst |
	mcst := (self bootstrapCacheDirectory / 'st-cache' / 'SUnit.st') asFileReference. 
	mcst ensureDelete.
	mcst parent ensureCreateDirectory.
	mcst writeStreamDo: [ :stream | | writer version |
		writer := MCStWriter on: stream.
		writer writeInitializers: false.

		version := (self originRepository loadVersionFromFileNamed: 'SUnit-Core.package').
		writer writeDefinitions: version snapshot definitions.

		version := (self originRepository loadVersionFromFileNamed: 'SUnit-Tests.package').
		writer writeDefinitions: version snapshot definitions.
	    
		stream nextPutAll: '		
		LongTestCase initialize.
		TestCase initialize.'.
	].