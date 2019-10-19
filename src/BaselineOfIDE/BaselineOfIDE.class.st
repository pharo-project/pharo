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

	self class environment at: #GTGenericStackDebugger ifPresent: [
	  Smalltalk tools register: GTGenericStackDebugger as: #debugger.
	  Smalltalk tools registeredDebuggersWithRanks.
	  Smalltalk tools debuggers add: ((self class environment at: #GTGenericStackDebugger)->3).
	  Smalltalk tools debuggers add: ((self class environment at: #GTSUnitDebugger)->70).
	  Smalltalk tools debuggers add: ((self class environment at: #GTBytecodeDebugger)->5).].

	  
	self class environment at: #GTPlayground ifPresent: [:playground | 
	  Smalltalk tools register: playground as: #workspace.].
	self class environment at: #GTInspector ifPresent: [:inspector | 
	  Smalltalk tools register: inspector as: #inspector.].

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

	RBProgramNode formatterClass: BIConfigurableFormatter.

	Color flushCache.

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
		spec postLoadDoIt: #'postload:package:'.
		
		spec baseline: 'BasicTools' with: [ spec repository: repository ].
		
		spec baseline: 'Athens' with: [ spec repository: repository ].
		spec baseline: 'Flashback' with: [ spec repository: repository ].

		spec baseline: 'Shift' with: [
			spec loads: 'shift-tests'. 
			spec repository: repository
		].

		spec baseline: 'Traits' with: [
			spec loads: 'traits-tests'. 
			spec repository: repository
		].
	
		spec baseline: 'Slot' with: [
			spec loads: 'slot-tests'. 
			spec repository: repository
		].

		spec baseline: 'Clap' with: [ spec repository: repository ].
	
		spec baseline: 'SUnit' with: [
			spec loads: 'Tests'. 
			spec repository: repository
		].
		
		spec package: 'Shout'.

		spec package: 'Rubric-Styling'.

		spec package: 'AST-Core-Tests'.
		
		spec baseline: 'BlueInk' with: [ 
			spec repository: repository; loads: #('Core'). ].
		
		spec package: 'Collections-Tests'.
		spec package: 'Text-Tests'.
		spec package: 'Network-Tests'.
		spec package: 'Network-Mail-Tests'.
		spec package: 'System-Object Events-Tests'.
		spec package: 'System-OSEnvironments-Tests'.
		spec package: 'Ring-Definitions-Tests-Core'.
		spec package: 'FileSystem-Tests-Core'.
		spec package: 'Fuel-Tools-Pharo'.
		spec package: 'Gofer-Tests'.
		spec package: 'Kernel-Tests'.
		spec package: 'Kernel-Tests-Extended'.
		spec package: 'Kernel-Tests-WithCompiler'..
		spec package: 'OpalCompiler-Tests'.
		spec package: 'ReleaseTests'.
		spec package: 'Ring-Definitions-Tests-Containers'.
		spec package: 'Ring-Definitions-Tests-Core'.
		
		spec package: 'Zodiac-Tests'.	
		spec package: 'Zodiac-Extra'.	
		spec package: 'Zinc-Zodiac'.	
			
		spec package: 'Metacello-TestsMCResources'.
		spec package: 'Metacello-TestsCore'.
		spec package: 'Metacello-TestsCommonMC'.
		spec package: 'Metacello-TestsMC'.	"standalone"
		spec package: 'Metacello-TestsMCCore'.	"standalone"
		spec package: 'Metacello-TestsPlatform'.	"standalone"
		spec package: 'Metacello-TestsReference'.	"standalone"
		spec package: 'Metacello-Gitlab-Tests'.

		spec package: 'MonticelloGUI-Tests'.
					
		spec package: 'FuzzyMatcher'.
		
		spec baseline: 'QAGT' with: [ spec repository: repository ].
		spec baseline: 'OSWindow' with: [ spec repository: repository ].
		spec baseline: 'Refactoring' with: [ spec repository: repository ].
		spec baseline: 'Catalog' with: [ spec repository: repository ].
		spec baseline: 'Epicea' with: [ spec repository: repository ].
		spec baseline: 'Misc' with: [ spec repository: repository ].
		spec baseline: 'GeneralTests' with: [ spec repository: repository ].

		spec baseline: 'SortFunctions' with: [ spec repository: repository ].

		"standalone packages"
		spec package: 'Announcements-Help'.
		spec package: 'Announcements-Core-Tests'.

		spec baseline: 'BlueInk' with: [ 
			spec repository: repository; loads: #('Extras' 'Tests'). ].
		
		spec package: 'CodeImport-Tests'.
		spec package: 'Compression-Tests'.
		spec package: 'FileSystem-Tests-Disk'.
		spec package: 'FileSystem-Tests-Attributes'.
		spec package: 'Files-Tests'.
		spec package: 'FuzzyMatcher-Tests'.
		spec package: 'Jobs-Tests'.
		spec package: 'Multilingual-Tests'.
		spec package: 'Pharo-Help'.
		spec package: 'Random-Tests'.
		spec package: 'ReflectionMirrors-Primitives-Tests'.
		spec package: 'Regex-Help'.
		spec package: 'Shout-Tests'.
		spec package: 'System-Platforms-Tests'.
		spec package: 'Zinc-Character-Encoding-Tests'.

		spec package: 'BaselineOfPharoBootstrap'.
		spec package: 'BaselineOfMonticello'.
		spec package: 'BaselineOfMetacello'.
		spec package: 'BaselineOfTelePharoSupport'.
		
		spec baseline: 'ReferenceFinder' with: [ spec repository: repository ].
		spec baseline: 'Refactoring2' with: [ spec repository: repository ].
		spec baseline: 'ClassParser' with: [ spec repository: repository ].

		spec baseline: 'Calypso' with: [ 
			spec
				repository: repository;
				loads: #('FullEnvironment' 'SystemBrowser' 'Tests'). ].

      spec baseline: 'Ring' with: [spec repository: repository].

      self groups: spec.
	].

]

{ #category : #baseline }
BaselineOfIDE >> groups: spec [ 
    
spec group: 'Mandatory-Group' with: #(
	'Fuel-Tools-Pharo'	"required by GT-Debugger HudsonBuildTools20"
	'Network-Tests'		"required by Multilingual-Tests"
	'Network-Mail-Tests'	"previously part of Network-Tests"
	'Gofer-Tests'		"required by EpiceaTests Metacello-TestsMC, depends on 'Metacello-Tests-Group', cleanup needed?"
). 

spec group: 'General-Help-Group' with: #(
	'Announcements-Help'
	'Pharo-Help'
	'Regex-Help'
).	

spec group: 'FuzzyMatcher-Core-Group' with: #(
	'FuzzyMatcher'
).
spec group: 'FuzzyMatcher-Tests-Group' with: #(
	'FuzzyMatcher-Tests'
).
spec group: 'FuzzyMatcher-Group' with: #(
	'FuzzyMatcher-Core-Group'
	'FuzzyMatcher-Tests-Group'
).

spec group: 'Zodiac-Extras-Group' with: #(
	'Zodiac-Extra'
	'Zinc-Zodiac'
).
spec group: 'Zodiac-Tests-Group' with: #(
	'Zodiac-Tests'
).
spec group: 'Zodiac-Group' with: #(
	'Zodiac-Extras-Group'
	'Zodiac-Tests-Group'
).
spec group: 'ReflectionMirrors-Tests-Group' with: #(
	'ReflectionMirrors-Primitives-Tests'
).
spec group: 'ReflectionMirrors-Group' with: #(
	'ReflectionMirrors-Tests-Group'
).

spec group: 'SmartSuggestions-Core-Group' with: #(
	"depends on 'Glamour-Core-Group', circular dependency, needs fix"
	'SmartSuggestions'
).
spec group: 'SmartSuggestions-Tests-Group' with: #(
	'SmartSuggestions-Tests'
).
spec group: 'SmartSuggestions-Group' with: #(
	'SmartSuggestions-Core-Group'
	'SmartSuggestions-Tests-Group'
).

spec group: 'Shout-Core-Group' with: #(
	'Shout'		"'Polymorph-Widgets' and 'NECompletion' depend on it, fix needed?"
	'Rubric-Styling'	"requires 'Shout'"      
).
spec group: 'Shout-Tests-Group' with: #(
	'Shout-Tests'
).
spec group: 'Shout-Group' with: #(
	'Shout-Core-Group'
	'Shout-Tests-Group'
).

spec group: 'Metacello-Tests-Group' with: #(
	'Metacello-TestsMCResources'
	'Metacello-TestsCore'
	'Metacello-TestsCommonMC'
	'Metacello-TestsMC'	
	'Metacello-TestsMCCore'	
	'Metacello-TestsPlatform'	
	'Metacello-TestsReference'
	'Metacello-Gitlab-Tests'
).

spec group: 'Kernel-Tests-Group' with: #(
	'CodeImport-Tests'	 
	'Collections-Tests'
	'Kernel-Tests' 
	'Kernel-Tests-Extended' 
	'Kernel-Tests-WithCompiler'
	'AST-Core-Tests' "requires BlueInk-Core"
	'Announcements-Core-Tests'
	'Compression-Tests' 
	'FileSystem-Tests-Core' 
	'FileSystem-Tests-Disk'
	'FileSystem-Tests-Attributes'
	'Files-Tests' 
	'Jobs-Tests' 
	'Multilingual-Tests'
	'OpalCompiler-Tests' 
	'Random-Tests'
	"'ReleaseTests'"
	'Ring-Definitions-Tests-Core'
	'Ring-Definitions-Tests-Containers' 
	'System-Object Events-Tests'
	'System-OSEnvironments-Tests'
	'Zinc-Character-Encoding-Tests' 
	'System-Platforms-Tests'
	'Text-Tests'		"required by Multilingual-Tests"
).

spec group: #default with: #(
	'BasicTools'	"dependency"
	'Athens'
	'Flashback'
	'Clap'
	
	'Mandatory-Group'

	'Kernel-Tests-Group'
	'BlueInk'

	'Shout-Group'

	"standalone:" 
	'Metacello-Tests-Group'		 
	'General-Help-Group'
	'ReflectionMirrors-Group'
	'Zodiac-Group'
	'FuzzyMatcher-Group'
	
	'QAGT'

	"standalone but dependent:"
	'OSWindow'
	'Refactoring'
	'Catalog'
	'Epicea'
	'Misc'
	'GeneralTests'
	'SortFunctions'	
	'ReferenceFinder'
	
	'BaselineOfPharoBootstrap'
	'BaselineOfMonticello'
	'BaselineOfMetacello'
	'BaselineOfTelePharoSupport'
	
	"Importing tests of Shift"
	'Shift'
	"Importing tests of Traits"
	'Traits'
	'Slot'
	
	'Refactoring2'
	'ClassParser'
	
	'Calypso'
	'Ring'
).

]

{ #category : #actions }
BaselineOfIDE >> loadIceberg [
	Metacello new
		baseline: 'Iceberg';
		repository: 'github://pharo-vcs/iceberg:v1.6.2';
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
	
	Smalltalk tools register: GTGenericStackDebugger as: #debugger.	

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
