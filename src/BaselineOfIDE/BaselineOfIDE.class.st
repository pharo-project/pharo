"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- BasicTools
- Traits
- Shift
- Slots
- Athens

Load type: 
- linear

Installs:
- Epicea
- decompiler
- tests
- refactoring
- GT Tools
- Versionner
- TxText
- OSWindow
- QualityAssistant
- some other packages
 
"
Class {
	#name : #BaselineOfIDE,
	#superclass : #BaselineOf,
	#classVars : [
		'Initialized'
	],
	#category : #BaselineOfIDE
}

{ #category : #actions }
BaselineOfIDE >> additionalInitialization [

	Smalltalk tools register: ExternalChangesBrowser as: #changeList.
	Smalltalk tools register: FileList as: #fileList.
	Smalltalk tools register: Finder as: #finder.
	Smalltalk tools register: ProcessBrowser as: #processBrowser.
	Smalltalk tools register: SyntaxErrorDebugger as: #syntaxErrorDebugger.
	Smalltalk tools register: TimeProfiler as: #timeProfiler.

	(MorphicCoreUIManager classPool at: #UIProcess) ifNotNil: [ :proc | proc terminate ].
	MorphicCoreUIManager classPool at: #UIProcess put: nil.

	PolymorphSystemSettings desktopColor:  Color veryVeryLightGray lighter.
	SourceCodeFonts setSourceCodeFonts: 10.
	FreeTypeSystemSettings loadFt2Library: true.
	FreeTypeSettings current monitorType: #LCD.
	FreeTypeSettings current glyphContrast: 55.
	
	RealEstateAgent usedStrategy: #cascadeFor:initialExtent:world:.
	GrowlMorph position: #bottomLeft.
	ShortcutReminder enabled: true.

	KMRepository reset.
	Morph shortcutsHandler: KMShortcutHandler new.

	MCSaveVersionDialog previousMessages add: String new.

	RBProgramNode formatterClass: (self class environment at: #EFFormatter ifAbsent: [RBSimpleFormatter]).

	Color flushCache.
	
	RubTextFieldArea defaultFindReplaceServiceClass: SpRubFindReplaceService.
	RubEditingArea defaultFindReplaceServiceClass: SpRubFindReplaceService.

	ASTTransformExamplePluginActive recompile.
	PharoCommandLineHandler recompile.
	SmalltalkImage recompile.
	
	RubCharacterScanner initialize.
	
	RubAbstractTextArea highlightMessageSend: true.
	
	PharoLightTheme beCurrent.
	
	SDL_Event initialize.
	
	HiRulerBuilderTest initialize.
		
	3 timesRepeat: [
		Smalltalk garbageCollect.
		Undeclared removeUnreferencedKeys.].
	
	"Making HeuristicCompletion the default completion engine"
	RubSmalltalkEditor completionEngineClass: CoCompletionEngine.
	
	self loadIceberg.	
	self loadExtraTools.	
	self makeCalypsoDefaultBrowser.
]

{ #category : #baselines }
BaselineOfIDE >> baseline: spec [
	<baseline>
	| repository |
	
	repository := self packageRepositoryURL.
	spec for: #common do: [ 
		spec postLoadDoIt: #postload:package:.
		spec baseline: 'BasicTools' with: [ spec repository: repository ].
		spec baseline: 'Athens' with: [ spec repository: repository ].
		spec baseline: 'Flashback' with: [ spec repository: repository ].
		spec baseline: 'Shift' with: [ 
			spec
				repository: repository;
				loads: 'shift-tests' ].

		spec baseline: 'Traits' with: [ 
			spec
				repository: repository;
				loads: 'traits-tests' ].

		spec baseline: 'Slot' with: [ 
			spec
				repository: repository;
				loads: 'slot-tests' ].

		spec baseline: 'Clap' with: [ 
			spec
				repository: repository;
				loads: 'development' ].

		spec baseline: 'SUnit' with: [ 
			spec
				repository: repository;
				loads: 'Tests' ].				

		spec baseline: 'EnlumineurFormatter' with: [ 
			spec
				repository: repository;
				loads: #( 'Tests' ) ].		
					
		spec baseline: 'Reflectivity' with: [ 
			spec
				repository: repository;
				loads: #( 'tests' ) ].
				

		spec package: 'Math-Operations-Extensions-Tests'.
		spec package: 'Network-Tests'.
		spec package: 'Network-Mail-Tests'.
		spec package: 'Gofer-Tests'.
		spec package: 'Rubric-SpecFindReplaceDialog'.
		spec baseline: 'Metacello' with: [ 
			spec
				repository: repository;
				loads: #( 'Tests' ) ].
		spec package: 'MonticelloGUI-Tests'.
		"Later we will load the UI of enlumineur probably here 
		"
		spec baseline: 'KernelTests' with: [ spec repository: repository ].
		spec baseline: 'Shout' with: [ spec repository: repository ].

		spec
			baseline: 'ReflectionMirrors'
			with: [ spec repository: repository ].
		spec baseline: 'FuzzyMatcher' with: [ spec repository: repository ].
		spec baseline: 'QA' with: [ spec repository: repository ].
		spec baseline: 'OSWindow' with: [ spec repository: repository ].
		spec baseline: 'EmergencyDebugger' with: [ spec repository: repository ].
		spec baseline: 'Refactoring' with: [ spec repository: repository ].
		spec baseline: 'Epicea' with: [ spec repository: repository ].
		spec baseline: 'Misc' with: [ spec repository: repository ].
		spec baseline: 'Fuel' with: [ 
			spec
				repository: repository;
				loads: #( 'Tests' ) ].
		spec baseline: 'GeneralTests' with: [ spec repository: repository ].
		spec baseline: 'FreeType' with: [ spec repository: repository; loads: 'tests' ].
		spec baseline: 'Keymapping' with: [ spec repository: repository; loads: 'tests' ].
		spec baseline: 'Zodiac' with: [ spec repository: repository ].
		spec baseline: 'SortFunctions' with: [ spec repository: repository ].
		spec baseline: 'Equals' with: [ spec repository: repository ].
		spec baseline: 'NewValueHolder' with: [ spec repository: repository; loads: 'tests' ].
		spec package: 'STON-Extensions'. 
		
		spec package: 'BaselineOfPharoBootstrap'.
		spec package: 'BaselineOfMonticello'.
		spec package: 'BaselineOfMetacello'.
		spec package: 'BaselineOfTelePharoSupport'.
		spec
			baseline: 'ReferenceFinder'
			with: [ spec repository: repository ].
		spec baseline: 'Refactoring2' with: [ spec repository: repository ].
		spec baseline: 'ClassParser' with: [ spec repository: repository ].
		spec baseline: 'Calypso' with: [ 
			spec
				repository: repository;
				loads: #( 'FullEnvironment' 'SystemBrowser' 'Tests' ) ].
		spec baseline: 'Ring' with: [ spec repository: repository ].
		spec baseline: 'HeuristicCompletion' with: [ spec repository: repository ].
		spec baseline: 'DependencyAnalyzer' with: [ spec repository: repository; loads: #('Tool-DependencyAnalyser-UI-Tab') ].
		self specCode: spec.
		self newTools: spec.
		self drTests: spec.	
		self microdown: spec.
		self beautifulComments: spec.
		self roassal3: spec.
	
		spec baseline: 'ThreadedFFI' with: [ spec repository: repository ].
		spec baseline: 'ProfilerUI' with: [ spec repository: repository ].
						
		spec package: 'Kernel-ExtraUtils' 
		
	]
]

{ #category : #baselines }
BaselineOfIDE >> beautifulComments: spec [ 

	spec baseline: 'BeautifulComments' with: [ 
		spec repository: (self class environment at: #BaselineOfPharo) beautifulCommentsRepository ]
]

{ #category : #baselines }
BaselineOfIDE >> drTests: spec [

	spec 
		baseline: 'DrTests' 
		with: [ spec repository: self packageRepositoryURL ]
]

{ #category : #actions }
BaselineOfIDE >> loadExtraTools [
	
	"Metacello new
		baseline: 'NewToolsDocumentationReader';
		repository: 'github://pharo-spec/NewTools-DocumentationReader:master/src';
		onConflictUseIncoming;
		load."
]

{ #category : #actions }
BaselineOfIDE >> loadIceberg [
	
	Metacello new
		baseline: 'Iceberg';
		repository: (self class environment at: #BaselineOfPharo) icebergRepository;
		onConflictUseLoaded;
		load.
	(Smalltalk classNamed: #Iceberg) enableMetacelloIntegration: true.
	
	self registerPharo.	
	self registerProject: 'Spec2' baseline: 'Spec2' otherBaselines: #('SpecCore').		
	self registerProject: 'NewTools'.		
	self registerProject: 'Roassal3'.		
	self registerProject: 'Microdown'.
	self registerProject: 'BeautifulComments'.		
	self registerIceberg
]

{ #category : #actions }
BaselineOfIDE >> makeCalypsoDefaultBrowser [

	(self class environment at: #ClyBrowserMorph) beAllDefault.
]

{ #category : #baselines }
BaselineOfIDE >> microdown: spec [ 

	spec baseline: 'Microdown' with: [ 
		spec 
			repository: (self class environment at: #BaselineOfPharo) microdownRepository;
			loads: #('RichText' 'Tests') ]
]

{ #category : #baselines }
BaselineOfIDE >> newTools: spec [ 

	spec baseline: 'NewTools' with: [
		spec 
			repository: (self class environment at: #BaselineOfPharo) newToolsRepository;
			loads: #('default') ].
]

{ #category : #'private - register' }
BaselineOfIDE >> pharoPluginClass [
	
	^ Smalltalk classNamed: #IcePharoPlugin
]

{ #category : #actions }
BaselineOfIDE >> postload: loader package: packageSpec [

	| initializersEnabled repo gofer |

	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].

	Author fullName: self class name.
	
	"collect and process the standard tools registrations"
	Smalltalk tools initDefaultToolSet.
	
	initializersEnabled := MCMethodDefinition initializersEnabled.

	MCMethodDefinition initializersEnabled: false.
	
	repo := TonelRepository new
		directory: self packageRepository directory;
		yourself.
		
	gofer := Gofer it repository: repo.
	gofer package: #'RPackage-Tests'.
	gofer load.
	
	MCMethodDefinition initializersEnabled: initializersEnabled.
	
	Smalltalk cleanOutUndeclared. 

	Stdio stdout 
		nextPutAll: ' ------------ Obsolete ------------';
		lf;
		nextPutAll: SystemNavigation default obsoleteClasses asString;
		lf;
		nextPutAll: ' ............ Obsolete ............';
		lf.

	Smalltalk fixObsoleteReferences.
	
	EpMonitor current enable.
		
	Author reset.

	MCMethodDefinition initializersEnabled: initializersEnabled.
	
	self additionalInitialization.

	Initialized := true.
]

{ #category : #actions }
BaselineOfIDE >> registerIceberg [
	
	self pharoPluginClass addIcebergProjectToIceberg.
	"Register baselines"
	Metacello new baseline: 'Tonel'; register.
	Metacello new baseline: 'LibGit'; register.
	Metacello new baseline: 'Iceberg'; register
]

{ #category : #actions }
BaselineOfIDE >> registerPharo [
	
	self pharoPluginClass addPharoProjectToIceberg
]

{ #category : #'private - register' }
BaselineOfIDE >> registerProject: projectName [

	^ self 
		registerProject: projectName 
		baseline: projectName 
		otherBaselines: #()
]

{ #category : #'private - register' }
BaselineOfIDE >> registerProject: projectName baseline: baselineName [

	^ self 
		registerProject: projectName 
		baseline: baselineName 
		otherBaselines: #()
]

{ #category : #'private - register' }
BaselineOfIDE >> registerProject: projectName baseline: baselineName otherBaselines: anArray [

	^ self 
		registerProject: projectName
		externalProject: projectName
		baseline: baselineName 
		otherBaselines: anArray
]

{ #category : #'private - register' }
BaselineOfIDE >> registerProject: projectName externalProject: externalProject [

	^ self 
		registerProject: projectName 
		externalProject: externalProject
		baseline: projectName 
		otherBaselines: #()
]

{ #category : #'private - register' }
BaselineOfIDE >> registerProject: projectName externalProject: externalProject baseline: baselineName otherBaselines: anArray [
	| baselineClass className |

	className := ('BaselineOf', baselineName) asSymbol.
	baselineClass := Smalltalk classNamed: className.
	baselineClass ifNil: [ ^ self ].

	self pharoPluginClass
		addProjectNamed: projectName
		commit: (self pharoPluginClass commitOfExternalProject: externalProject)
		baselines: { className }
		tags: #(#system).
	"Register baselines"
	({baselineName}, anArray) do: [ :each |
		Metacello new baseline: each; register ]
]

{ #category : #baselines }
BaselineOfIDE >> roassal3: spec [

	spec 
		baseline: 'Roassal3' 
 		with: [ 
			spec 
				repository: (self class environment at: #BaselineOfPharo) roassal3Repository;
    			loads: #( 'Core' 'Tests') ]
]

{ #category : #baselines }
BaselineOfIDE >> specCode: spec [

	spec baseline: 'SpecCode' with: [ 
		spec
			repository: (self class environment at: #BaselineOfPharo) specRepository;
			className: 'BaselineOfSpec2';
			loads: #('Code' 'CodeTests') ]
]
