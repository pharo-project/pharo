preparation
exportCoreTestsInStFile

	| mcst blacklistedClasses blacklistedMethods | 
	
	blacklistedClasses := #(
		ASTTransformExamplePluginActive
		MCServerRegistry
		EventManagerTest
		RPackageMCSynchronisationTest
		RPackageCategorySynchronisationTest
		RPackageClassesSynchronisationTest
		RPackageExtensionMethodsSynchronisationTest
		RPackageMethodsSynchronisationTest
		RPackageMonticelloSynchronisationTest
		RPackageTraitSynchronisationTest
		OSEnvironmentTest
	).	
		
	blacklistedMethods := #(
		testInBlockArgumentInstanceVariableShadowing
		testInBlockTempArgumentShadowing
		testInBlockTempInstanceVariableShadowing
		testInBlockTempShadowing
		testInstanceVariableShadowing
		testReservedNameAsBlockArgumentShadowing
		testReservedNameAsMethodArgumentShadowing
		testSiblingBlocksInstanceVariableShadowing
		
		testComplexIsSelfEvaluating
		testExportAMethodWithLangTag
		testSplitStringOnRegex
		
		testEUCJP
		testPercentEncodingHiragana
		testMultistringFont
		testOpening
		testClassWithPluginEnabled
		testUnescapePercentsWithTextEncoding
		testMcWorkingCopy
		testMcPackage
		
		testDeepStructure
		
		testFindTokens
		testFindTokensIncludes
		testNext
		testPeek
		testPeekFor
		testReset
		testUpToEnd
		testImportAMethodWithLangTag
		testUTF16Back
		testUTF8Back
		testFindTokensEscapedBy
		testShuffled
		testAtRandom
		testBlockEscape
		testCannotReturn
		testCannotReturn
		testIsTerminatingNormalTermination
		testWaitAndWaitTimeoutTogether	
	).		
	
	mcst := (self bootstrapCacheDirectory / 'st-cache' / 'CoreTests.st') asFileReference.
	mcst ensureDelete.
	mcst parent ensureCreateDirectory.
	mcst writeStreamDo: [ :stream | | writer version |
		writer := MCStWriter on: stream.
		writer writeInitializers: false.
		
		#('RPackage-Tests' 'Collections-Tests' 'Kernel-Tests' 'AST-Tests-Core' 'Announcements-Tests-Core'  'FileSystem-Tests-Core' 
		'FileSystem-Tests-Disk' 'Files-Tests' 'JobsTests'  'Multilingual-Tests' 'OpalCompiler-Tests'  'Random-Tests' 'ReleaseTests'
		'Ring-Tests-Kernel'  'Ring-Tests-Containers' 'Zinc-Character-Encoding-Tests' 'Tests' 'FileSystem-Memory')  do: [ :packageName |
			| definitions |
			version := (self originRepository loadVersionFromFileNamed: packageName, '.package').
			definitions := version snapshot definitions.
			definitions := definitions reject: [:def | blacklistedClasses includes: def className ].
			definitions := definitions reject: [:def | def isMethodDefinition and: [blacklistedMethods includes: def selector] ].
			writer writeDefinitions: definitions.].
	].