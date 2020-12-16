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
	Smalltalk tools register: RecentMessageList as: #recentMessageList.
	Smalltalk tools register: SyntaxErrorDebugger as: #syntaxErrorDebugger.
	Smalltalk tools register: TimeProfiler as: #timeProfiler.

	(MorphicCoreUIManager classPool at: #UIProcess) ifNotNil: [ :proc | proc terminate ].
	MorphicCoreUIManager classPool at: #UIProcess put: nil.

	PolymorphSystemSettings desktopColor:  Color veryVeryLightGray lighter.
	SourceCodeFonts setSourceCodeFonts: 10.
	LogicalFontManager unload.
	FreeTypeSystemSettings loadFt2Library: true.
	FreeTypeSettings current monitorType: #LCD.
	FreeTypeSettings current glyphContrast: 55.
	
	CatalogSettings displayCatalogProjectsInSpotter: false.

	RealEstateAgent usedStrategy: #cascadeFor:initialExtent:world:.
	GrowlMorph position: #bottomLeft.
	ShortcutReminder enabled: true.

	KMRepository reset.
	Morph shortcutsHandler: KMShortcutHandler new.

	MCSaveVersionDialog previousMessages add: String new.

	RBProgramNode formatterClass: (self class environment at: #BIConfigurableFormatter ifAbsent: [RBSimpleFormatter]).

	Color flushCache.
	
	RubTextFieldArea defaultFindReplaceServiceClass: SpRubFindReplaceService.
	RubEditingArea defaultFindReplaceServiceClass: SpRubFindReplaceService.

	ASTTransformExamplePluginActive recompile.
	PharoCommandLineHandler recompile.
	SmalltalkImage recompile.
	
	RubCharacterScanner initialize.
	
	RubAbstractTextArea highlightMessageSend: true.
	
	PharoDarkTheme beCurrent.
	
	SDL_Event initialize.
	
	HiRulerBuilderTest initialize.
	
	3 timesRepeat: [
		Smalltalk garbageCollect.
		Undeclared removeUnreferencedKeys.].
	
	self loadIceberg.	
	self makeCalypsoDefaultBrowser.
]

{ #category : #baseline }
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
		
		spec baseline: 'BlueInk' with: [ 
 			spec repository: repository; loads: #('Tests') ].				

		spec baseline: 'EnlumineurFormatter' with: [ 
			spec
				repository: repository;
				loads: #( 'Tests' 'UI-Tests' ) ].
				
		spec package: 'Network-Tests'.
		spec package: 'Network-Mail-Tests'.
		spec package: 'Gofer-Tests'.
		spec package: 'System-Hashing-Testing'.
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
		spec baseline: 'GeneralHelp' with: [ spec repository: repository ].
		spec
			baseline: 'ReflectionMirrors'
			with: [ spec repository: repository ].
		spec baseline: 'FuzzyMatcher' with: [ spec repository: repository ].
		spec baseline: 'QA' with: [ spec repository: repository ].
		spec baseline: 'OSWindow' with: [ spec repository: repository ].
		spec baseline: 'Refactoring' with: [ spec repository: repository ].
		spec baseline: 'Epicea' with: [ spec repository: repository ].
		spec baseline: 'Misc' with: [ spec repository: repository ].
		spec baseline: 'Fuel' with: [ 
			spec
				repository: repository;
				loads: #( 'Tests' ) ].
		spec baseline: 'GeneralTests' with: [ spec repository: repository ].
		spec baseline: 'Zodiac' with: [ spec repository: repository ].
		spec baseline: 'SortFunctions' with: [ spec repository: repository ].
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
		spec baseline: 'PharoGlamour' with: [ spec repository: repository ].
		spec baseline: 'PharoGT' with: [ spec repository: repository ].
		spec baseline: 'Catalog' with: [ spec repository: repository ].
		spec package: 'Pharo-WelcomeHelp' ]
]

{ #category : #actions }
BaselineOfIDE >> loadIceberg [
	Metacello new
		baseline: 'Iceberg';
		repository: 'github://pharo-vcs/iceberg:v1.6.10';
		onConflictUseLoaded;
		load.
	(Smalltalk classNamed: #Iceberg) enableMetacelloIntegration: true.
	(Smalltalk classNamed: #IcePharoPlugin) addPharoProjectToIceberg.
]

{ #category : #actions }
BaselineOfIDE >> makeCalypsoDefaultBrowser [

	(self class environment at: #ClyBrowserMorph) beAllDefault.
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
