"
I contain tests that validate the integrity of the system.
"
Class {
	#name : 'ReleaseTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#category : 'ReleaseTests-Release',
	#package : 'ReleaseTests',
	#tag : 'Release'
}

{ #category : 'accessing' }
ReleaseTest class >> actualProcesses [
	^ Process allSubInstances reject: [ :each | each isTerminated ]
]

{ #category : 'accessing' }
ReleaseTest class >> defaultTimeLimit [

	^ 2 minutes
]

{ #category : 'accessing' }
ReleaseTest >> actualProcesses [
	^ self class actualProcesses
]

{ #category : 'accessing' }
ReleaseTest >> allowedObsoletes [
	"take obsoletes from a configuration variable if it exist. 
	 this is used because some CI (e.g. iceberg) require the project to be unloaded 
	 and after reload to properly test, and this can left some obsoletes.
	 WARNING: This has to be used carefully. it may lead to obsoletes to be left in 
	 system incorrectly."
	| allowed |
			
	allowed := Smalltalk os environment 
		at: 'PHARO_CI_ALLOWED_OBSOLETES'
		ifAbsent: [ nil ].
	allowed ifNil: [ ^ #() ].
			
	^ (allowed substrings: ', ') collect: [ :each |  'AnObsolete', each ]
]

{ #category : 'helpers' }
ReleaseTest >> assertValidLintRule: aLintRule [
	self assertValidLintRule: aLintRule withExceptions: {}
]

{ #category : 'helpers' }
ReleaseTest >> assertValidLintRule: aLintRule withExceptions: someNames [
	| runner results |
	runner := ReSmalllintChecker new.
	runner
		rule: {aLintRule};
		environment: RBBrowserEnvironment default;
		run.

	results := (runner criticsOf: aLintRule) reject: [ :critique | someNames includes: critique entity name ].

	self
		assert: results isEmpty
		description: [ String
				streamContents: [ :s |
					s
						<< aLintRule rationale;
						lf;
						<< 'Violations: ';
						lf.
					results
						do: [ :e |
							s
								<< '- ';
								print: e entity ]
						separatedBy: [ s lf ] ] ]
]

{ #category : 'helpers' }
ReleaseTest >> hasStartUpOrShutDownMethod: aClass [
	| keySelectors |
	keySelectors := #(#startUp #startUp: #shutDown #shutDown:).
	^ (aClass selectors includesAny: keySelectors)
		or: [ aClass class selectors includesAny: keySelectors ]
]

{ #category : 'accessing' }
ReleaseTest >> knownProcesses [
	"Return a dynamic list of know processes"

	"(self actualProcesses difference: self knownProcesses) do: [ :each | each suspend; terminate ]."

	^ {
		Processor activeProcess.
		Processor backgroundProcess.
		FinalizationProcess runningFinalizationProcess.
		SmalltalkImage current lowSpaceWatcherProcess.
		MorphicUIManager uiProcess.
		CurrentExecutionEnvironment value watchDogProcess.
		Delay schedulingProcess.
		TKTWatchDog onDuty pharoProcess.
		OSSDL2Driver eventLoopProcess.
		TFCallbackQueue uniqueInstance callbackProcess } asSet
]

{ #category : 'tests - variables' }
ReleaseTest >> testAllClassPoolBindingAreClassVariables [
	| wrong |
	wrong := OrderedCollection new.
	testingEnvironment allClasses do: [ :class | wrong addAll: (class classVariables reject: [ :each | each isKindOf: ClassVariable ]) ].
	self assertEmpty: wrong
]

{ #category : 'tests - variables' }
ReleaseTest >> testAllClassPoolBindingHaveDefiningClass [
	| wrong |
	wrong := OrderedCollection new.
	testingEnvironment allClasses do: [ :class | wrong addAll: (class classVariables reject: [ :each | each definingClass == class ]) ].
	self assertEmpty: wrong
]

{ #category : 'tests - variables' }
ReleaseTest >> testAllGlobalBindingAreGlobalVariables [
	| wrong |
	wrong := testingEnvironment associations reject: [ :each | each isKindOf: GlobalVariable ].
	self assertEmpty: wrong
]

{ #category : 'tests - variables' }
ReleaseTest >> testAllInstanceVariablesDefiningClass [
	"Make sure that all slots (ivars) have a correct defining class
	This should never happen but the test is a good way to avoid future bugs"
	| violating |

	violating := Smalltalk globals allBehaviors select: [ :class |
		class localSlots anySatisfy: [:var | var definingClass ~= class ]].

	self
		assert: violating isEmpty
		description: 'Slot with wrong defining Class in: ', violating asArray asString
]

{ #category : 'tests - variables' }
ReleaseTest >> testAllInstanceVariablesOwningClass [
	"Make sure that all slots (ivars) have a correct owning class
	This should never happen but the test is a good way to avoid future bugs"
	| violating |

	violating := Smalltalk globals allBehaviors select: [ :class |
		class slots anySatisfy: [:var | var owningClass ~= class ]].

	self
		assert: violating isEmpty
		description: 'Slot with wrong owning Class in: ', violating asArray asString
]

{ #category : 'tests - variables' }
ReleaseTest >> testAllInstanceVariablesStartLowercase [
	"Make sure that all class instance variable names start with a lowercase letter"
	| violating |

	violating := Smalltalk globals allBehaviors select: [ :class |
		class slots anySatisfy: [:var | var name first isUppercase]].

	self
		assert: violating isEmpty
		description: 'Instance variable names must start with a lowercase letter: ', violating asArray asString
]

{ #category : 'tests - variables' }
ReleaseTest >> testAllSharedVariablesOwningClass [
	"Make sure that all SharedVariables (ClassVariables) have a correct owning class
	This should never happen but the test is a good way to avoid future bugs"
	| violating |

	violating := Smalltalk globals allBehaviors select: [ :class |
		class classVariables anySatisfy: [:var | var owningClass ~= class instanceSide]].

	self
		assert: violating isEmpty
		description: 'ClassVar with wrong owning Class in: ', violating asArray asString
]

{ #category : 'tests - variables' }
ReleaseTest >> testClassesShadow [

	| classes |
	classes := Smalltalk globals allBehaviors select: [ :class |
		           class definedVariables anySatisfy: [ :var |
			           var isShadowing ] ].

	self assert: classes isEmpty description: classes asArray asString
]

{ #category : 'tests - package' }
ReleaseTest >> testExistingPackageNamesDoesNotContainIllegalCharacters [

	| illegalCharacters |
	illegalCharacters := #( $\ $/ $: $* $? $" $< $> $| ).

	self packageOrganizer packages do: [ :package |
		self deny: (package name includesAnyOf: illegalCharacters).
		package tags do: [ :tag | self deny: (tag name includesAnyOf: illegalCharacters) ] ]
]

{ #category : 'tests' }
ReleaseTest >> testExplicitRequirementMethodsShouldBeImplementedInTheirUsers [
	"If a class is using a trait with an explicit requirement method, this class should implement the method"

	self assertValidLintRule: ReExplicitRequirementMethodsRule new
]

{ #category : 'tests' }
ReleaseTest >> testInstanceSideMethodsWithNilKeyInLastLiteral [
	| instanceSideMethodsWithNilKeyInLastLiteral |

	instanceSideMethodsWithNilKeyInLastLiteral := SystemNavigation new instanceSideMethodsWithNilKeyInLastLiteral.

	self
		assert: instanceSideMethodsWithNilKeyInLastLiteral isEmpty
		description: [ String streamContents: [ :s|
			s
				nextPutAll: 'Found methods with nil as last literal: ';
				print: instanceSideMethodsWithNilKeyInLastLiteral ]]
]

{ #category : 'tests' }
ReleaseTest >> testKeyClassesArePresentInStartupList [
	| keyClasses registeredHandlers |
	keyClasses := #(#Delay #ProcessorScheduler #Stdio #OSPlatform #UUIDGenerator #DiskStore #FinalizationProcess #BasicCommandLineHandler).
	registeredHandlers := SessionManager default startupList
		collect: [:each | each handledId].

	keyClasses do:	[ :className |
		self assert: (registeredHandlers includes: className).
		self assert: (self hasStartUpOrShutDownMethod: (testingEnvironment at: className)) ].

	self assert: (registeredHandlers includes: #ErrorHandlerSessionHandler)
]

{ #category : 'tests' }
ReleaseTest >> testLayoutIntegrity [

	SystemNavigation default allClassesDo: [ :class |
		class instanceSide classLayout checkIntegrity.
		class classSide classLayout checkIntegrity ]
]

{ #category : 'tests' }
ReleaseTest >> testLocalMethodsOfTheClassShouldNotBeRepeatedInItsTraits [
	"If a class is using a trait with an explicit requirement method, this class should implement the method"

	self assertValidLintRule: ReLocalMethodsSameThanTraitRule new withExceptions: #(RBSmalllintTestLocalMethodsSameThanTraitObject)
]

{ #category : 'tests' }
ReleaseTest >> testManifestNamesAccordingToPackageNames [
	"Make sure package name and manifest name are in synch"

	|manifestClasses actualManifestNames expectedManifestNames |
	manifestClasses := self class environment allClasses select: [:each | each isManifest ].
	actualManifestNames := (manifestClasses collect: [:each | each name ]) sorted.
	expectedManifestNames := (manifestClasses collect: [:each | TheManifestBuilder manifestClassNameFor: each package name ]) sorted.

	self
		assert: actualManifestNames size
		equals: expectedManifestNames size.

	actualManifestNames do: [:each |
		 self assert: (expectedManifestNames includes: each)]
]

{ #category : 'tests' }
ReleaseTest >> testMethodsContainNoHalt [

	| methods |
	methods := SystemNavigation new allMethods select: [ :method | method containsHalt ].
	"these methods are using halt for testing something"
	methods := methods reject: [ :method |
		           method hasPragmaNamed: #haltOrBreakpointForTesting ].
	"these methods are implementing halt, we are not interested"
	methods := methods reject: [ :method |
		           method hasPragmaNamed: #debuggerCompleteToSender ].

	"there should be no method left"
	self assert: methods isEmpty description: [
		String streamContents: [ :stream |
			stream
				nextPutAll: 'Found methods with halt';
				print: methods ] ]
]

{ #category : 'tests - announcer' }
ReleaseTest >> testNoDeadSubscriptions [

	| dead |
	"we skiped this on the ci in the past: https://github.com/pharo-project/pharo/issues/2471"
	3 timesRepeat: [ Smalltalk garbageCollect ].

	dead := self class codeChangeAnnouncer subscriptions subscriptions select: [ :sub | sub subscriber isNil ].

	self assert: dead asArray equals: #(  )
]

{ #category : 'tests' }
ReleaseTest >> testNoEmptyPackages [
	"Test that we have no empty packages left"

	| violating |
	violating := self packageOrganizer packages select: #isEmpty.
	self assertEmpty: violating
]

{ #category : 'tests - methods' }
ReleaseTest >> testNoEquivalentSuperclassMethods [

	| methods |
	"we do not care about methods that are installed from traits"
	methods := SystemNavigation new allMethods reject: [ :method | method isFromTrait ].

	methods := methods select: [ :method |
		           method overriddenMethod
			           ifNotNil: [ :overridenMethod | method equivalentTo: overridenMethod ]
			           ifNil: [ false ] ].

	self assert: methods size <= 305
]

{ #category : 'tests - variables' }
ReleaseTest >> testNoLiteralIsPinnedInMemory [
	| methodsWithPinnedLiterals |

	methodsWithPinnedLiterals := SystemNavigation default allMethods flatCollect: [ :each |
	(each allLiterals select: [ :eachLiteral | eachLiteral isPinnedInMemory ])
		ifNotEmpty: [ { each } ]
		ifEmpty: [ #() ] ].

	self assertEmpty: methodsWithPinnedLiterals
]

{ #category : 'tests' }
ReleaseTest >> testNoNilAssignmentInInitializeMethod [

	self assertValidLintRule: ReNoNilAssignationInInitializeRule new
]

{ #category : 'tests - source' }
ReleaseTest >> testNoNullCharacter [
	"Check that we do not have NULL in sources - see https://github.com/pharo-project/pharo/issues/9631"

	| violations |
	violations := SystemNavigation default allMethods select: [ :m | m sourceCode includes: Character null ].
	self assert: violations isEmpty description: 'Source corrupted: Methods with Null character found'
]

{ #category : 'tests' }
ReleaseTest >> testNoOrphanPackage [
	"This test ensure that all packages loaded in Pharo are part of a baseline. This will be the most useful when we will be able to execute release tests last in the CI of Pharo. We will be able to detect generated packages that are not removed by the #tearDowns"

	| declaredPackages |
	declaredPackages := ((self class environment allClasses select: [ :class | class inheritsFrom: BaselineOf ]) flatCollect: [ :baseline |
		                     baseline withAllPackageNames ]) asSet.
	self assertEmpty: (self packageOrganizer packages reject: [ :package | package isUndefined or: [ declaredPackages includes: package name ] ])
]

{ #category : 'tests - package' }
ReleaseTest >> testNoPackagesOverride [
	"Class side packages should not be overridden"

	Object allSubclasses do: [ :each | each class compiledMethodAt: #packages ifPresent: [ self fail: 'The #packages method should not be overriden' ] ]
]

{ #category : 'tests - source' }
ReleaseTest >> testNoPeriodInMethodSignature [
	| methods |
	methods := SystemNavigation new allMethods select: [ :method |
 		method sourceCode lines first trimRight last == $..].

	self assert: methods isEmpty description: [
		String streamContents: [ :stream |
			stream
				nextPutAll: 'Found methods with period in signature:';
				print: methods ] ]
]

{ #category : 'tests - variables' }
ReleaseTest >> testNoShadowedVariablesInMethods [
	"Fail if there are methods who define shadowed temps or args"
	| found validExceptions remaining |
	found := SystemNavigation default methods select: [ :m |
		m isQuick not and: [ "quick methods do not define variables"
			m ast variableDefinitionNodes anySatisfy: [ :node | node variable isShadowing ] ]].
	"No other exceptions beside the ones mentioned here should be allowed"
	validExceptions := {
		RBSmalllintTestObject>>#tempVarOverridesInstVar.
		ReTempVarOverridesInstVarRuleTest>>#sampleMethod:}.

	remaining := found asOrderedCollection
								removeAll: validExceptions;
								yourself.

	self
		assert: remaining isEmpty
		description: ('the following methods have shadowing variable definitions and should be cleaned: ', remaining asString)
]

{ #category : 'tests' }
ReleaseTest >> testNoSuperInitializeOnClassSideInitialization [

	self assertValidLintRule: ReDoNotSendSuperInitializeInClassSideRule new
]

{ #category : 'tests' }
ReleaseTest >> testObsoleteClasses [
	| obsoleteClasses |

	Smalltalk fixObsoleteReferences.
	Smalltalk garbageCollect.
	obsoleteClasses := SystemNavigation new obsoleteClasses
		reject: [ :each | each isAnonymous or: [ (self allowedObsoletes includes: each name) ] ].

	self
		assert: obsoleteClasses isEmpty
		description: [
			String streamContents: [ :s|
				s
					nextPutAll: 'Obsolete classes remaining: ';
					print: obsoleteClasses ]]
]

{ #category : 'tests - announcer' }
ReleaseTest >> testOnlyWeakSubscriptions [

	| strong |
	"only weak subscriptions should be allowed to be added to the SystemAnnouncer..."
	strong := self class codeChangeAnnouncer subscriptions subscriptions reject: [ :each | each isKindOf: WeakAnnouncementSubscription ].

	self assert: strong asArray equals: #(  )
]

{ #category : 'tests - package' }
ReleaseTest >> testPackageOrganizer [
	"Ensure other tests temporary created organizers are collected"

	3 timesRepeat: [ Smalltalk garbageCollect ].

	"Now check :)"
	self
		assert: PackageOrganizer allInstances size = 1
		description: 'There are multiple (' , PackageOrganizer allInstances size asString , ') instances of PackageOrganizer'.

	self
		assert: PackageOrganizer allInstances first == self packageOrganizer
		description: 'The default package organizer is the not the only instance of PackageOrganizer'
]

{ #category : 'tests' }
ReleaseTest >> testPharoVersionFileExists [

	"Test there is a pharo.version file next to this image containing the short version of this pharo image.
	This file is required by the Pharo launcher to correctly detect the Pharo version we are running on."

	| pharoVersionFile |
	pharoVersionFile := FileLocator imageDirectory / 'pharo.version'.
	self assert: pharoVersionFile exists.
	self
		assert: pharoVersionFile readStream contents trimBoth
		equals: SystemVersion current major asString,  SystemVersion current minor asString
]

{ #category : 'tests - package' }
ReleaseTest >> testProperManifestClasses [

	|manifests|
	manifests := self class environment allClasses select: [:each | each isManifest ].
	self assert: (manifests allSatisfy: [:each | each inheritsFrom: PackageManifest ]).
	self assert: (manifests allSatisfy: [:each | each name beginsWith: 'Manifest' ])
]

{ #category : 'tests' }
ReleaseTest >> testRatioOfCommentedClasses [

	| stereotypes violating |
	self skip. "skip for now as it is failing on the CI for Newtools"
	stereotypes := OrderedCollection new.
	stereotypes
		addAll: Smalltalk globals allTraits;
		addAll: Smalltalk globals allClasses.

	stereotypes removeAll: TestCase allSubclasses.
	stereotypes removeAll: BaselineOf allSubclasses.
	violating := stereotypes select: [ :each | each comment isEmpty ].
	self assert: violating size <= 876
]

{ #category : 'tests' }
ReleaseTest >> testShouldFileLocatorBeBeforeSystemSettingsPersistenceInStartupList [
	"This test documents issue https://pharo.manuscript.com/f/cases/17721"

	| startupList settingsIndex |
	startupList := (SessionManager default startupList collect: [:each | each handledId]).
	settingsIndex := startupList indexOf: #SystemSettingsPersistence.
	settingsIndex > 0
		ifTrue: [ self should: [ (startupList indexOf: #FileLocator) < (startupList indexOf: #SystemSettingsPersistence) ] ]
]

{ #category : 'tests' }
ReleaseTest >> testShouldWorldMorphBeAfterFontClassesInStartupList [
	"This test documents issue https://pharo.manuscript.com/f/cases/17834"
	"The Startup of he WorldMorph is now done in the UIManagerHandler"
	| startupList |
	startupList := (SessionManager default startupList collect: [:each | each handledId]).
	#(StrikeFont LogicalFont FreeTypeSettings FreeTypeCache)
		do: [ :fontClass |
			self should: [ (startupList indexOf: #ErrorHandlerSessionHandler) > (startupList indexOf: #SystemSettingsPersistence) ] ]
]

{ #category : 'tests' }
ReleaseTest >> testTestCasesEndsWithTestOrTestCase [
	self assertValidLintRule: ReTestClassNameShouldEndWithTestRule new
]

{ #category : 'tests - package' }
ReleaseTest >> testThatAllMethodsArePackaged [
	| classes instanceMethods classMethods allMethods methodsWithoutPackageInfo |
	classes := Smalltalk allClassesAndTraits.
	instanceMethods := classes flatCollect: #methods.
	classMethods := classes flatCollect: [ :class | class classSide methods ].
	allMethods := instanceMethods , classMethods.
	methodsWithoutPackageInfo := allMethods select: [ :method | method package isNil ].
	self assertEmpty: methodsWithoutPackageInfo
]

{ #category : 'tests - methods' }
ReleaseTest >> testThatThereAreNoSelectorsRemainingThatAreSentButNotImplemented [
	"There is a long term cleanup to fix 'selectors send but not implemented'.
	 This test makes sure that we do not introduce new cases until we are down to one"

	| knownFailures violations wantedAndValidViolations |

	"String streamContents: [ :aStream | aStream << '#( '.  SystemNavigation default allSentButNotImplementedSelectors do: [ :m | .aStream << '''' << m printString  << ''' ' ]. aStream << ')' ]"
	knownFailures := #( 'AthensCCWArcSegment>>#accept:' 'AthensCWArcSegment>>#accept:' 'AthensCairoCanvas>>#visitCubicSegment:'
	                    'AthensCairoCanvas>>#visitQuadSegment:' 'AthensCloseSegment>>#accept:' 'AthensCubicSegment>>#accept:' 'AthensEllipticalArcSegment>>#accept:'
	                    'AthensInteractiveScene>>#eventHandledByScene:' 'AthensLineSegment>>#accept:' 'AthensMoveSegment>>#accept:' 'AthensParagraph>>#compose:style:from:in:'
	                    'AthensPolygon>>#paintFillsUsing:on:' 'BalloonEdgeData>>#stepToFirstScanLine' 'BalloonEdgeData>>#stepToNextScanLine'
	                    'BalloonEngine>>#registerFills:' 'BalloonFillData>>#computeFill' 'ClyFullBrowserMorph>>#extendClassScopeForMethods:'
	                    'ComplexBorderStyle>>#drawPolyPatchFrom:to:on:usingEnds:' 'ComplexBorderStyle>>#drawLineFrom:to:on:'
	                    'Context>>#simulatePrimitive:in:receiver:arguments:' 'Context>>#doPrimitive:method:receiver:args:'
	                    'Context>>#failPrimitiveWith:' 'DAPackageAnalyzerDiffTreePresenter>>#buildRoots' 'DTReRunConfiguration>>#items'
	                    'DebugSession>>#shouldDisplayContext:basedOnFilters:'
	                    'EDEmergencyDebugger>>#resetDisplay' 'EDTest>>#prepareMethodVersionTest' 'ExternalDropHandler class>>#defaultHandler'
	                    'FFICallbackArgumentReader>>#stackPointer' 'FFIFloat32>>#callbackReturnOn:for:' 'FFIFloatType>>#callbackReturnOn:for:'
	                    'FFIIndirectFunctionResolution>>#resolveFunction:' 'FileDialogWindow>>#saveForcedSelectedFile' 'FileList class>>#morphicViewOnFile:contents:fileList:'
	                    'GLMOSWindowWorldMorph>>#delete' 'IRReconstructor>>#fixPushNilsForTemps' 'IceLibgitCommitWalk>>#shouldInclude:'
	                    'IcePackage>>#outgoingCommits' 'IceRemote>>#upstreamForBranch:backend:' 'IceTag>>#lastCommit' 'IceTipHiedraAltComponentHistoryBrowser>>#newCommitRow:commit:'
	                    'IceTipHiedraAltHistoryBrowser>>#newCommitRow:commit:' 'IceTipHiedraAltHistoryBrowser>>#initializeCommitList'
	                    'InstructionStream>>#interpretNext3ByteSistaV1Instruction:for:extA:extB:startPC:'
	                    'KMDispatcher>>#spotterForKeysFor:' 'KMMetaModifier>>#printOsRepresentationOn:' 'LGitDiff>>#includesFileNamed:'
	                    'LGitRepository>>#cloneRemote:signature:branch:checkoutOptions:' 'LazyListMorph>>#drawOnAthensCanvas:'
	                    'MCAddition>>#myVersionOn:' 'MCFileTreeAbstractReader>>#packageProperties' 'MCModification>>#myVersionOn:' 'MCOrganizationDefinition>>#addAnalysisToPackageChangeSet:change:'
	                    'MicrodownSpecComponentForTest class>>#open' 'MockLessonView>>#showTutorialNode:' 'MorphTreeMorph>>#nodeExpandRequest:'
	                    'MorphTreeMorph>>#selectItemsRequest:' 'PSMCClassChangeWrapper>>#remoteChosen' 'PolygonMorph>>#intersects:' 'RBBasicDummyLintRuleTest class>>#createMatcherFor:method:'
	                    'RBBasicDummyLintRuleTest class>>#usesAdd' 'RBBasicDummyLintRuleTest class>>#unreferencedVariables'
	                    'RBBasicDummyLintRuleTest class>>#classNotReferenced' 'RBBasicDummyLintRuleTest class>>#canCall:in:from:'
	                    'RBBasicDummyLintRuleTest class>>#tempsReadBeforeWritten' 'RBBasicDummyLintRuleTest class>>#usesTrue'
	                    'RBBasicDummyLintRuleTest class>>#variableAssignedLiteral' 'RBBasicDummyLintRuleTest class>>#onlyReadOrWritten'
	                    'RBBasicDummyLintRuleTest class>>#sentNotImplementedInApplication' 'RBBasicDummyLintRuleTest class>>#tempVarOverridesInstVar'
	                    'RBBasicDummyLintRuleTest class>>#instVarInSubclasses' 'RBBasicDummyLintRuleTest class>>#utilityMethods'
	                    'RBBasicLintRuleTestData class>>#canCall:in:from:' 'RBBasicLintRuleTestData class>>#createMatcherFor:method:'
	                    'RBBasicLintRuleTestData class>>#sentNotImplementedInApplication' 'RBBasicLintRuleTestData class>>#utilityMethods'
	                    'RBClassDataForRefactoringTest>>#inlineComponent' 'RBClassDataForRefactoringTest>>#textInput:name:symbol:'
	                    'RBClassDataForRefactoringTest>>#inlineFailed' 'RBClassDataForRefactoringTest>>#renderContentOn:'
	                    'RBEnvironmentVisitor>>#visitPackageEnv:' 'RBEnvironmentVisitor>>#visitClassEnv:' 'RBEnvironmentVisitor>>#visitProtocolEnv:'
	                    'RBFindAndReplacePreviewPresenter>>#initializeDialogWindow:' 'RBLintRuleTestData>>#runOnEnvironment:'
	                    'RBRemoveClassRefactoring>>#senders' 'RBTransformationDummyRuleTest class>>#initializeAfterLoad1'
	                    'RBTransformationDummyRuleTest class>>#rewrite:methods:name:' 'RBTransformationRuleTestData class>>#initializeAfterLoad1'
	                    'RBTransformationRuleTestData class>>#rewrite:methods:name:' 'RGChunkImporter>>#visitDoItChunk:' 'ReSentNotImplementedRuleTest>>#sampleMethod'
	                    'Rectangle>>#newRectButtonPressedDo:inWorld:' 'RubSegmentMorph>>#intersects:' 'RubTextSegmentMorph>>#displayIconAt:onAthensCanvas:'
	                    'SpApplication>>#ensureConfiguration' 'SpApplicationBackend>>#notify:' 'SpChangeSorterPresenter>>#createRingMethodForRemovedSelector:inClass:'
	                    'SpRenameDefaultSpecToDefaultLayoutRefactoring>>#preconditions' 'SpRenameDefaultSpecToDefaultLayoutRefactoring>>#initialize'
	                    'SpTransmission>>#from:port:to:transform:postTransmission:' 'SpTreeColumn>>#rowMorphFor:' 'SpUIThemeDecoratorTest>>#testDoesNotUnderstand'
	                    'SpWindowPresenter>>#notify:' 'StSpotterPragmaBasedProcessor>>#installKeymappingsOn:'
	                    'StringMorph>>#launchEditor:' 'SycRemoveKeepingSubclassesClassStrategy>>#execute:' 'TransferMorph>>#justDroppedInto:event:'
	                    'TransferMorph>>#delete' 'ZnConstants class>>#frameworkMCVersion' ).

	violations := (SystemNavigation default allSentButNotImplementedSelectors collect: [ :method | method printString ]) asOrderedCollection.
	"This next line is to ignore this for SmalltalkCI and not make external Pharo projects fail :("
	violations removeAllSuchThat: [ :violation | violation beginsWithAnyOf: #( SCI SmalltalkCI ) ].

	"There is a violation that we explicitly want for rule tesing"
	wantedAndValidViolations := #( 'ReSentButNotUnderstoodBySuperRuleTest>>#sampleMethod' ).

	violations removeAll: wantedAndValidViolations.
	self assertEmpty: (violations difference: knownFailures)
]

{ #category : 'tests - variables' }
ReleaseTest >> testUndeclared [

	| undeclaredVariables validExceptions remaining description |
	[ "we compile a second method with the undeclared #undeclaredStubInstVar1 to trigger the code path of removing twice in #cleanOutUndeclared"
	self class compile: 'methodForTest ^undeclaredStubInstVar1'.
	Smalltalk cleanOutUndeclared.
	undeclaredVariables := Undeclared associations select: [ :each | each isUndeclaredVariable ].

	validExceptions := { #undeclaredStubInstVar1. #undeclaredStubInstVar2. #Gofer }. "The laste one is for Smalltalk CI and our external projects... But we should find a better solution at some point."

	"for now we filter by name, maybe filtering by variable would be better"
	remaining := undeclaredVariables reject: [ :each | validExceptions includes: each name ].

	"we look for one of the using methods of the undeclared var and report that,
	this should be enough to fix it quickly"
	description := String streamContents: [ :stream |
		               stream nextPutAll: 'Found undeclared Variables: '.
		               remaining do: [ :variable |
			               | method |
			               method := variable usingMethods anyOne.
			               stream cr
				               nextPutAll: variable name;
				               nextPutAll: ' in: ';
				               print: method methodClass;
				               nextPutAll: '>>';
				               print: method selector ] ].

	self assert: remaining isEmpty description: description ] ensure: [ self class removeSelector: #methodForTest ]
]

{ #category : 'tests - package' }
ReleaseTest >> testUndefinedPackageShouldBeEmpty [
	"The unpackage package should not have any defined class or extended classes"

	self assertEmpty: self packageOrganizer undefinedPackage classes
]

{ #category : 'tests' }
ReleaseTest >> testUnknownProcesses [
	"Make sure that there are no other processes running except the known processes"

	| unknownProcesses |
	unknownProcesses := self actualProcesses difference: self knownProcesses.
	self
		assert: unknownProcesses isEmpty
		description: (String streamContents: [ :out |
			out << 'Found unknown processes: '.
			unknownProcesses
				do: [ :each |
					out
						print: each name;
						print: ':';
						print: each ]
				separatedBy: [ out << String crlf ] ])
]

{ #category : 'tests - package' }
ReleaseTest >> testUnpackagedClasses [

	| unpackagedClasses |
	unpackagedClasses := Smalltalk allClassesAndTraits select: [ :each | each package isUndefined ].
	self assert: unpackagedClasses isEmpty description: (String streamContents: [ :s |
			 s nextPutAll: 'Found the following unpackaged classes: '.
			 unpackagedClasses do: [ :cls | s tab print: cls ] separatedBy: [ s cr ] ])
]

{ #category : 'tests' }
ReleaseTest >> testVersionInImageHeaderIsCorrect [

	| currentVersion |
	currentVersion := SystemVersion current.

	self
		assert: Smalltalk vm imageVersionInImageHeader
		equals: currentVersion major * 10 + currentVersion minor
]

{ #category : 'tests' }
ReleaseTest >> testWorldMenuHasHelpForAllEntries [
	"In this test we check that at least every terminal menu entry of the world menu has an help."

	| menuElements |
	menuElements := WorldState new menuBuilder itemList.

	"Here we have the roots of the menu. We want the terminal menu entries."
	[ menuElements allSatisfy: [ :each | each itemList isNil ] ]
		whileFalse: [ menuElements := menuElements inject: OrderedCollection new into: [ :coll :each | each itemList ifNil: [ coll add: each ] ifNotNil: [ :items | coll addAll: items ]. coll ] ].

	menuElements collect: #spec thenDo: [ :item | self assert: item help isNotNil description: item label , ' menu entry in world menu should have an help.' ]
]
