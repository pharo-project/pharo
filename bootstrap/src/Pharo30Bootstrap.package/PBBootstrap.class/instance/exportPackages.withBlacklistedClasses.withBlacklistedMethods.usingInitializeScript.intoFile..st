preparation
exportPackages: packageList withBlacklistedClasses: blacklistedClasses withBlacklistedMethods: methods usingInitializeScript: aString intoFile: aFileName

	| mcst | 	
	mcst := (self bootstrapCacheDirectory / 'st-cache' / aFileName) asFileReference.
	mcst ensureDelete.
	mcst parent ensureCreateDirectory.
	mcst writeStreamDo: [ :stream | | writer version |
		writer := MCStWriter on: stream.
		writer writeInitializers: false.
		
		packageList do: [ :packageName |
			| definitions |
			version := (self originRepository loadVersionFromFileNamed: packageName, '.package').
			definitions := version snapshot definitions.
			definitions := definitions reject: [:def | blacklistedClasses includes: def className ].
			definitions := definitions reject: [:def | def isMethodDefinition and: [methods includes: def selector] ].
			writer writeDefinitions: definitions ].
		"Write initialization instructions"
		stream nextPutAll: aString.
	].