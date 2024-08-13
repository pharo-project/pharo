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
	#name : 'BaselineOfIDE',
	#superclass : 'BaselineOf',
	#instVars : [
		'repository'
	],
	#classVars : [
		'Initialized'
	],
	#category : 'BaselineOfIDE-Baseline',
	#package : 'BaselineOfIDE',
	#tag : 'Baseline'
}

{ #category : 'baselines' }
BaselineOfIDE >> baseline: spec [

	<baseline>
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
		spec postLoadDoIt: #postload:package:.
		spec baseline: 'BasicTools' with: [ spec repository: repository ].
		spec
			package: 'Tool-MorphicProfiler'
			with: [ spec requires: #( 'BasicTools' ) ].
		spec baseline: 'Athens' with: [ spec repository: repository ].
		spec baseline: 'Flashback' with: [ spec repository: repository ].

		self
			load: 'Shift' group: 'shift-tests' spec: spec;
			load: 'Traits' group: 'traits-tests' spec: spec;
			load: 'Slot' group: 'slot-tests' spec: spec;
			load: 'Clap' group: 'development' spec: spec;
			load: 'SUnit' group: 'Tests' spec: spec;
			load: 'EnlumineurFormatter' group: 'Tests' spec: spec;
			load: 'Reflectivity' group: 'tests' spec: spec;
			load: 'DebugPoints' group: 'Tests' spec: spec;
			load: 'Refactoring' group: 'Tests' spec: spec.

		spec package: 'Math-Operations-Extensions-Tests'.
		spec package: 'Network-Tests'.
		spec package: 'Network-Mail-Tests'.
		spec package: 'Rubric-SpecFindReplaceDialog'.

		spec package: 'OpalCompiler-UI'.

		self load: 'Metacello' group: 'Tests' spec: spec.

		spec package: 'MonticelloGUI-Tests'.
		"Later we will load the UI of enlumineur probably here
		"
		spec baseline: 'Shout' with: [ spec repository: repository ].

		spec
			baseline: 'ReflectionMirrors'
			with: [ spec repository: repository ].
		spec baseline: 'FuzzyMatcher' with: [ spec repository: repository ].
		spec baseline: 'QA' with: [ spec repository: repository ].
		spec baseline: 'OSWindow' with: [ spec repository: repository ].
		spec
			baseline: 'EmergencyDebugger'
			with: [ spec repository: repository ].

		self load: 'Epicea' group: 'Browsers' spec: spec.

		spec baseline: 'Misc' with: [ spec repository: repository ].

		self load: 'Fuel' group: 'Tests' spec: spec.

		spec baseline: 'GeneralTests' with: [ spec repository: repository ].

		self load: 'FreeType' group: 'tests' spec: spec.
		self load: 'Keymapping' group: 'tests' spec: spec.

		spec baseline: 'Zodiac' with: [ spec repository: repository ].
		spec baseline: 'SortFunctions' with: [ spec repository: repository ].
		spec baseline: 'Equals' with: [ spec repository: repository ].

		self load: 'NewValueHolder' group: 'tests' spec: spec.
		spec package: 'STON-Extensions'.

		spec package: 'BaselineOfPharoBootstrap'.
		spec package: 'BaselineOfMonticello'.
		spec package: 'BaselineOfMetacello'.
		spec
			baseline: 'ReferenceFinder'
			with: [ spec repository: repository ].
		spec baseline: 'ClassParser' with: [ spec repository: repository ].


		self
			load: 'Calypso'
			group: #( 'FullEnvironment' 'SystemBrowser' 'Tests' )
			spec: spec.
		spec baseline: 'Ring' with: [ spec repository: repository ].
		spec
			baseline: 'HeuristicCompletion'
			with: [ spec repository: repository ].
		self
			load: 'DependencyAnalyzer'
			group: 'Tool-DependencyAnalyser-UI-Tab'
			spec: spec.
		self specCode: spec.
		self newTools: spec.
		self drTests: spec.
		self microdown: spec.
		spec package: #'BeautifulComments'  with: [ spec requires:  #( 'Microdown') ].
		self documentBrowserCore: spec.
		self welcomeBrowser: spec.
		self roassal: spec.
		
		spec package: 'DarkBlueTheme'.

		spec baseline: 'ThreadedFFI' with: [ spec repository: repository ].

		spec package: 'Kernel-ExtraUtils' ]
]

{ #category : 'baselines - dependencies' }
BaselineOfIDE >> documentBrowserCore: spec [

	spec baseline: 'NewToolsDocumentBrowser' with: [
		spec
			repository: (self class environment at: #BaselineOfPharo) documentBrowserRepository;
			loads: 'Core+Tests' ]
]

{ #category : 'baselines - dependencies' }
BaselineOfIDE >> drTests: spec [

	spec
		baseline: 'DrTests'
		with: [ spec repository: (self packageRepositoryURLForSpec: spec) ]
]

{ #category : 'private - loading' }
BaselineOfIDE >> load: aProject group: aGroupName spec: spec [

	spec baseline: aProject with: [
		spec
			repository: (self packageRepositoryURLForSpec: spec);
			loads: aGroupName ]
]

{ #category : 'actions' }
BaselineOfIDE >> loadExtraTools [
	"Load NewTools-DocumentBrowser-GitHubResource packages depending on Iceberg GitHub plugin"

	Metacello new
		baseline: 'NewToolsDocumentBrowser';
		repository: (self class environment at: #BaselineOfPharo)
				documentBrowserRepository;
		onConflictUseIncoming;
		load.

	"Hack: Document Browser is loaded in 2 steps because of the dependency of some packages to Iceberg.
	The 2nd load is done through Metacello that adds new project registrations in Iceberg (user, not system).
	We then remove these last registrations."
	#('NewTools-DocumentBrowser' 'iceberg') do: [ :repositoryName | | foundRepo |
		foundRepo := IceRepository registry 
			detect: [ :repo | repo name = repositoryName ]
			ifNone: [ self error: repositoryName , ' not found in Iceberg registry!' ].
		IceRepository unregisterRepository: foundRepo ifAbsent: [ "ignore" ] ].
]

{ #category : 'actions' }
BaselineOfIDE >> loadIceberg [

	Metacello new
		baseline: 'Iceberg';
		repository: (self class environment at: #BaselineOfPharo) icebergRepository;
		onConflictUseLoaded;
		load.
	(Smalltalk classNamed: #Iceberg) enableMetacelloIntegration: true.

	Smalltalk os environment at: #GITHUB_TOKEN ifPresent: [ :token |
		| credentials |
		credentials := (Smalltalk classNamed: #IceTokenCredentials) new
			               username:
				               (Smalltalk os environment
					                at: #GITHUB_USER
					                ifAbsent: [ self error: 'Github token was found but not the github user associated to this token.' ]);
			               token: token;
			               host: 'github.com';
			               yourself.

		(Smalltalk classNamed: #IceCredentialStore) current storeCredential: credentials forHostname: 'github.com'.
		'Using authentification for Github API' traceCr ].

	self registerPharo.
	self registerProject: 'Spec2' baseline: 'Spec2' otherBaselines: #('SpecCore').
	self registerProject: 'NewTools'.
	self registerProject: 'Roassal'.
	self registerProject: 'Microdown'.
	self registerProject: 'DocumentBrowser' baseline: 'NewToolsDocumentBrowser' otherBaselines: #().
	self registerProject: 'WelcomeBrowser'.
	self registerIceberg
]

{ #category : 'actions' }
BaselineOfIDE >> makeCalypsoDefaultBrowser [

	(self class environment at: #ClyBrowserMorph) beAllDefault.
]

{ #category : 'baselines - dependencies' }
BaselineOfIDE >> microdown: spec [ 

	spec baseline: 'Microdown' with: [ 
		spec 
			repository: (self class environment at: #BaselineOfPharo) microdownRepository;
			loads: #('RichText' 'Tests') ]
]

{ #category : 'baselines - dependencies' }
BaselineOfIDE >> newTools: spec [ 

	spec baseline: 'NewTools' with: [
		spec 
			repository: (self class environment at: #BaselineOfPharo) newToolsRepository;
			loads: #('default') ].
]

{ #category : 'private - register' }
BaselineOfIDE >> pharoPluginClass [
	
	^ Smalltalk classNamed: #IcePharoPlugin
]

{ #category : 'actions' }
BaselineOfIDE >> postload: loader package: packageSpec [
	"Ignore pre and post loads if already executed"

	Initialized = true ifTrue: [ ^ self ].

	"collect and process the standard tools registrations"
	Smalltalk tools initDefaultToolSet.

	EpMonitor current enable.

	Smalltalk tools register: ExternalChangesBrowser as: #changeList.
	Smalltalk tools register: FileList as: #fileList.
	Smalltalk tools register: Finder as: #finder.
	Smalltalk tools register: ProcessBrowser as: #processBrowser.

	(MorphicCoreUIManager readClassVariableNamed: #UIProcess) ifNotNil: [ :proc | proc terminate ].
	MorphicCoreUIManager writeClassVariableNamed: #UIProcess   value: nil.

	PolymorphSystemSettings desktopColor: Color veryVeryLightGray lighter.
	SourceCodeFonts setSourceCodeFonts: 10.
	FreeTypeSystemSettings loadFt2Library: true.
	FreeTypeSettings current monitorType: #LCD.
	FreeTypeSettings current glyphContrast: 55.

	RealEstateAgent usedStrategy: #cascadeFor:initialExtent:world:.
	GrowlMorph position: #bottomLeft.
	ShortcutReminder enabled: true.

	KMRepository reset.
	Morph shortcutsHandler: KMShortcutHandler new.

	ASTProgramNode formatterClass: (self class environment at: #EFFormatter ifAbsent: [ RBSimpleFormatter ]).

	Color flushCache.

	RubTextFieldArea defaultFindReplaceServiceClass: SpRubFindReplaceService.
	RubEditingArea defaultFindReplaceServiceClass: SpRubFindReplaceService.

	RubCharacterScanner initialize.

	RubAbstractTextArea highlightMessageSend: true.

	DarkBlueTheme beCurrent.

	SDL_Event initialize.

	HiRulerBuilderTest initialize.

	"Making HeuristicCompletion the default completion engine"
	RubSmalltalkEditor completionEngineClass: CoCompletionEngine.

	self loadIceberg.
	self loadExtraTools.
	self makeCalypsoDefaultBrowser.

	Initialized := true
]

{ #category : 'actions' }
BaselineOfIDE >> registerIceberg [
	
	self pharoPluginClass addIcebergProjectToIceberg.
	"Register baselines"
	Metacello new baseline: 'Tonel'; register.
	Metacello new baseline: 'LibGit'; register.
	Metacello new baseline: 'Iceberg'; register
]

{ #category : 'actions' }
BaselineOfIDE >> registerPharo [
	
	self pharoPluginClass addPharoProjectToIceberg
]

{ #category : 'private - register' }
BaselineOfIDE >> registerProject: projectName [

	^ self 
		registerProject: projectName 
		baseline: projectName 
		otherBaselines: #()
]

{ #category : 'private - register' }
BaselineOfIDE >> registerProject: projectName baseline: baselineName [

	^ self 
		registerProject: projectName 
		baseline: baselineName 
		otherBaselines: #()
]

{ #category : 'private - register' }
BaselineOfIDE >> registerProject: projectName baseline: baselineName otherBaselines: anArray [

	^ self 
		registerProject: projectName
		externalProject: projectName
		baseline: baselineName 
		otherBaselines: anArray
]

{ #category : 'private - register' }
BaselineOfIDE >> registerProject: projectName externalProject: externalProject [

	^ self 
		registerProject: projectName 
		externalProject: externalProject
		baseline: projectName 
		otherBaselines: #()
]

{ #category : 'private - register' }
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

{ #category : 'baselines - dependencies' }
BaselineOfIDE >> roassal: spec [

	spec 
		baseline: 'Roassal' 
 		with: [ 
			spec 
				repository: (self class environment at: #BaselineOfPharo) roassalRepository;
    			loads: #( 'Core' 'Tests') ]
]

{ #category : 'baselines - dependencies' }
BaselineOfIDE >> specCode: spec [

	spec baseline: 'SpecCode' with: [ 
		spec
			repository: (self class environment at: #BaselineOfPharo) specRepository;
			className: 'BaselineOfSpec2';
			loads: #('Code' 'CodeTests') ]
]

{ #category : 'baselines - dependencies' }
BaselineOfIDE >> welcomeBrowser: spec [

	spec baseline: 'WelcomeBrowser' with: [
		spec repository: (self class environment at: #BaselineOfPharo) welcomeBrowserRepository ]
]
