preparation
exportMonticelloInStFile

	| mcst |
	mcst := (self bootstrapCacheDirectory / 'st-cache' / 'Monticello.st') asFileReference.
	mcst ensureDelete.
	mcst parent ensureCreateDirectory.
	mcst writeStreamDo: [ :stream | | writer version |
		writer := MCStWriter on: stream.
		writer writeInitializers: false.

		version := (self originRepository loadVersionFromFileNamed: 'Compression.package').
		writer writeDefinitions: version snapshot definitions.

		version := (self originRepository loadVersionFromFileNamed: 'Monticello.package').
		writer writeDefinitions: version snapshot definitions.

		version := (self originRepository loadVersionFromFileNamed: 'Ring-Monticello.package').
		writer writeDefinitions: version snapshot definitions.
	    
		stream nextPutAll: '		
		ZipConstants initialize.
		ZipFileConstants initialize.
		ZipWriteStream initialize.
		GZipConstants initialize.
		InflateStream initialize.
		FastInflateStream initialize.
		
		MCCacheRepository initialize.
		MCPackageManager initialize.
		MCLazyVersionInfo initialize.
		MCMethodDefinition initialize.
		MCDataStream initialize.
		MCWorkingCopy initialize.'.
	].