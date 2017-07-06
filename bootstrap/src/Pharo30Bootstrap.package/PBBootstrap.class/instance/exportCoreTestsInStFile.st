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
		
'testActiveComment' 'testAddClassAfterMethods' 'testAddClassAfterMethodsAtMetaclassToo' 'testAddClassDefinitionNoDuplicate' 'testAddMethod' 'testAddRemoveMethod' 'testAddRemoveSelector' 'testAddRemoveSelectorOfMetaclass' 'testAddTag' 'testAddTagNames' 'testAddTagsToAClass' 'testAllRangesOfSubstring' 'testAllSendersOfASelector' 'testAnalogousCodeTo' 'testAnd' 'testAnnouncementClassRemovedIsRaisedOnRemoveFromSystem' 'testArgumentNamesExistingMethod' 'testAsActive' 'testAsLowercase' 'testAsUppercase' 'testBasicRemoveClass' 'testBehaviorRespectsPolymorphismWithTraitBehavior' 'testBitOr' 'testBogusClassAddition' 'testBreadthFirstSelect' 'testByteEncoderFromUrl' 'testCache' 'testCaseSensitiveLessOrEqual' 'testClassAddition' 'testClassComment' 'testClassDefinitionRemoval' 'testClassDefinitionRemovalName' 'testClassDefinitionWithTagsRemoval' 'testClassDescriptionRespectsPolymorphismWithTraitDescription' 'testClassIsExtendedInPackage' 'testClassNamesAndExtensionSelectorsDo2' 'testClassRespectsPolymorphismWithTrait' 'testClasses' 'testClassesAndExtensionMethodsDo' 'testCodePointEncodingDecoding' 'testCodePointStreams' 'testCollectArguments' 'testCommentFromSourceFile' 'testComparingCaseInsensitiveLatin1' 'testComparingTrees' 'testCompiledMethodIsDefinedInPackage' 'testCompiledMethodIsExtensionInPackage' 'testCompiledMethodPackageFromOrganizer' 'testConvencienceMethods' 'testCopy' 'testCopyInContext' 'testDNU' 'testDefinedClassesAndDefinedClassNames' 'testDefinedSelectors' 'testDefinedSelectorsForClass' 'testEmbeddingCompressedSourceCode' 'testEmbeddingSourceCode' 'testEmpty' 'testEqualToWithMapping' 'testExtendingPackagesOfClass' 'testExtensionClassNames' 'testExtensionClasses' 'testExtensionClassesWithCompiledMethod' 'testExtensionMethodOfClass' 'testExtensionMethods' 'testExtensionSelectors' 'testExtensionSelectorsForClass' 'testFindAnySubstringStartingAt' 'testFindDelimitersStartingAt' 'testFindStringStartingAt' 'testFindStringStartingAtCaseSensitive' 'testFormatter' 'testHome' 'testIdentityIndexOf' 'testIfTrueEffect' 'testIfTrueValue' 'testIfTrueWithClosureAfterJump' 'testIncludeClass' 'testIncludeClassMore' 'testIncludeSelectorOfClass' 'testIncludeSelectorOfMetaClass' 'testIncludesMethodOfClassInPresenceOfOtherPackageExtensions' 'testIncludesOrTouches' 'testIncludesSubstringCaseSensitive' 'testIndexOf' 'testIndexOfFirstUppercaseCharacter' 'testIndexOfWideCharacterFromTo' 'testInstalledMethodsWithIncorrectTrailerKind' 'testIsAllAlphaNumerics' 'testIsMessageSentInSystemAnswersMethodReference' 'testIsMessageSentInSystemWithClassesActuallySendngTheMessage' 'testIsMessageSentInSystemWithTheSelectorInsideAnArray' 'testIsPrime' 'testIsPrime2' 'testIsProbablyPrime' 'testIsWideString' 'testJump' 'testKeyClassesArePresentInStartupList' 'testLastSpacePosition' 'testLatin1Encoder' 'testLeadingCharRunLengthAt' 'testLoremIpsum2048' 'testMatchInContext' 'testMetaclassAndTraitClassRespectsPolymorphismRules' 'testMetaclassHasExtensions' 'testMethodAddition' 'testMethodClass' 'testMethodPackageFromObsoleteClass' 'testMethodPackageOfRemovedClass' 'testMethodPackageResolution' 'testMethodSourceNodeAtPC' 'testMethods' 'testMethodsForClass' 'testMethodsWithUnboundGlobals' 'testMoveTo' 'testNextMatchAll' 'testNextPut' 'testNextPutAll' 'testNextPutAllStartingAt' 'testNextWithAnUnknownCharacterGetError' 'testNonAscii' 'testObjects' 'testObsoleteClasses' 'testOneGigAllocation' 'testOnlyLetters' 'testOr' 'testOriginWithRequiredMethodInTraitChain' 'testOutOfMemorySignal' 'testPackageOfClassForClassesNotDefinedInPackageButJustExtendingIt' 'testPackageOfClassForDefinedClasses' 'testPackageOfClassMethodFromTraitExtensionIsExtendingPackage' 'testPackageOfClassMethodFromTraitIsTraitPackage' 'testPackageOfClassMethodIsClassPackage' 'testPackageOfTraitMethodIsTraitPackage' 'testPackagesOfClass' 'testPostorderSelect' 'testPreorderSelect' 'testPrimitiveNamedErrorCode2' 'testPrimitives' 'testPrivateClassRegisterUnregister' 'testProtocol' 'testPutSingle2ByteEncodedCharacter' 'testReadIntoStartingAtCount' 'testReadIntoStartingAtCountFromStream' 'testReadIntoStartingAtCountFromStreamWithOffset' 'testReferencedClasses' 'testRemoveClassAfterMethods' 'testRemoveClassRemovesExtensions' 'testRemoveExtensionMethodRemovesExtensionsFromRPackage' 'testRemoveMethod' 'testRemoveMethodDoesNotRemoveFromClass' 'testRemoveShouldRaiseErrorWhenMethodIsNotInPackage' 'testRemoveTag' 'testRemoveTaggedClasses' 'testRenamePackage' 'testRenamePackageToOwnTagName' 'testRenamePackageToOwnTagNameWithClassesInRoot' 'testRenameToMakesMCDirty' 'testReplaceAssignment' 'testReplaceBlock' 'testSelectors' 'testSelectorsForClass' 'testSemaphoreNoTimeout' 'testSemaphoreTimeout' 'testShouldWorldMorphBeAfterFontClassesInStartupList' 'testSimpleSourceMapping' 'testSizeInMemoryOfCharacters' 'testSizeInMemorySmallIntegers' 'testSourceNodeExecuted' 'testSourceNodeOptimized' 'testStartUpList' 'testStartingSituation' 'testThatAllMethodsArePackaged' 'testThreadSafe' 'testTimeStamp' 'testTraitCompositionModifications' 'testTraitsUsersSanity' 'testTranslateToLowercase' 'testTranslateToUppercase' 'testTruncate' 'testUTF16EncoderBigEndian' 'testUTF16EncoderLittleEndian' 'testUTF32EncoderSimple' 'testUndeclared' 'testUniqueClassInDefinedClassesUsingAddClassDefinition' 'testUnknownProcesses' 'testUnpackagedClasses' 'testUnregisterPackage' 'testUpToAnyOfDo' 'testindexOfstartingAt' 'testDetectEncoding'	
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
