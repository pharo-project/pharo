preparation
exportHudsonBuildToolsInStFile

	| mcst blacklistedClasses blacklistedMethods | 
	
	blacklistedClasses := #(
		HDChangeReport
		HDLintReport
		HDCoverageReport
		HDTestCoverage
	).	
		
	blacklistedMethods := #().		
	
	mcst := (self bootstrapCacheDirectory / 'st-cache' / 'HudsonBuildTools.st') asFileReference.
	mcst ensureDelete.
	mcst parent ensureCreateDirectory.
	mcst writeStreamDo: [ :stream | | writer version |
		writer := MCStWriter on: stream.
		writer writeInitializers: false.
		
		#('HudsonBuildTools20') do: [ :packageName |
			| definitions |
			version := (self originRepository loadVersionFromFileNamed: packageName, '.package').
			definitions := version snapshot definitions.
			definitions := definitions reject: [:def | blacklistedClasses includes: def className ].
			definitions := definitions reject: [:def | def isMethodDefinition and: [blacklistedMethods includes: def selector] ].
			writer writeDefinitions: definitions.].
	].