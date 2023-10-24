"
I know how to create classes in the system.
The classes are nor installed or modifies other objects. That is part of the job of the ShiftClassInstaller.

I can be extended by using a different builder enhancer. 
See ShDefaultBuilderEnhancer for a default implementation. 

I can be used directly to create anonymous classes, but it is better if you use the anonymous class installer.

I also can compare the old class with the configured new class to calculate the required changes.

I'm responsible to build class expressed with the fluid syntax.

```st
Object << #Point 
	slots: { #x . #y };
	tag: 'Basics';
	package: 'Kernel'
```	
	
### Design decisions

The KEY design decisions are:
- The code snippet above should be able to be used for scripting (interactively).
- The code snippet above must return a class builder 
- A class builder should offer two important messsage: 
-- `build` that returns a class but does not install it. 
-- `install` that installs the class in the system.

To manage class side slots and traits, I get all the instance side information in addition to the class side one.
For this the fillFromClass: method is responsible to convert all the class information into an information expected by the class builder. 

In the future we would like to revisit the need for such fluid class builders and also revisit the API of the class builder.There are too many conversion and we should realigned the APIs with the class default values too. 
"
Class {
	#name : 'ShiftClassBuilder',
	#superclass : 'Object',
	#instVars : [
		'buildEnvironment',
		'installingEnvironment',
		'superclassName',
		'name',
		'layoutDefinition',
		'comment',
		'commentStamp',
		'superclass',
		'newMetaclass',
		'newClass',
		'oldClass',
		'oldMetaclass',
		'builderEnhancer',
		'metaclassClass',
		'extensibleProperties',
		'changeComparers',
		'changes',
		'metaSuperclass',
		'superclassResolver',
		'inRemake',
		'package',
		'tag'
	],
	#classVars : [
		'BuilderEnhancer'
	],
	#category : 'Shift-ClassBuilder',
	#package : 'Shift-ClassBuilder'
}

{ #category : 'defaults' }
ShiftClassBuilder class >> defaultBuildEnhancer [
	^ BuilderEnhancer ifNil: [ ShDefaultBuilderEnhancer ]
]

{ #category : 'class initialization' }
ShiftClassBuilder class >> initialize [

	self deprecatedAliases: #( #FluidBuilder #FluidClassBuilder #FluidTraitBuilder )
]

{ #category : 'accessing' }
ShiftClassBuilder class >> setDefaultBuilderEnhancer: aClass [
	BuilderEnhancer := aClass
]

{ #category : 'changes' }
ShiftClassBuilder >> addChange: aChange [
	changes add: aChange
]

{ #category : 'changes' }
ShiftClassBuilder >> addChangeComparer: aChangeComparer [
	changeComparers add: aChangeComparer
]

{ #category : 'accessing' }
ShiftClassBuilder >> allSlots [

	^ self builderEnhancer allSlotsForBuilder: self
]

{ #category : 'building' }
ShiftClassBuilder >> build [

	self tryToFillOldClass.
	self detectBuilderEnhancer.
	self builderEnhancer validateRedefinition: self oldClass.

	self validateSuperclass.
	self compareWithOldClass.

	self layoutDefinition copySlotsIfUsedIn: oldClass.

	"If this is the first build, when building a class.
	We need to check if there is no conflicts with existing subclasses.
	If we are in a remake, it have been done when building the superclass modified before."

	self isInRemake ifFalse: [ self layoutDefinition validate ].

	self createMetaclass.
	self createClass.

	self oldClass ifNotNil: [
		self newClass basicCategory: self oldClass category.
		self copyProtocols.
		self newClass commentSourcePointer: self oldClass commentSourcePointer ].

	self createSharedVariables.

	self installSlotsAndVariables.

	self oldClass ifNotNil: [ self builderEnhancer compileMethodsFor: self ].

	self builderEnhancer afterMethodsCompiled: self.
	^ newClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> buildEnvironment [
	^ buildEnvironment
]

{ #category : 'accessing' }
ShiftClassBuilder >> buildEnvironment: anObject [
	buildEnvironment := anObject
]

{ #category : 'accessing' }
ShiftClassBuilder >> builderEnhancer [
	^ builderEnhancer ifNil: [ self detectBuilderEnhancer ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> builderEnhancer: anObject [
	builderEnhancer := anObject.

	builderEnhancer initializeBuilder: self
]

{ #category : 'accessing' }
ShiftClassBuilder >> category [

	self flag: #package. "Category is an old concept mixing packages and tag. For now this is what is used by the system but in the future we should go away from it."
	^ self tag
		  ifNotNil: [ :packageTag | self package , '-' , packageTag ]
		  ifNil: [ self package ]
]

{ #category : 'deprecated' }
ShiftClassBuilder >> category: aString [

	| converter |
	self flag: #package. "To deprecate once it is not use in the bootstrap anymore."
	converter := CategoryConverter category: aString environment: self installingEnvironment.
	self package: converter packageName.
	self tag: converter tagName
]

{ #category : 'accessing' }
ShiftClassBuilder >> changeComparers [
	^ changeComparers
]

{ #category : 'accessing' }
ShiftClassBuilder >> changes [
	^ changes
]

{ #category : 'accessing' }
ShiftClassBuilder >> classNamed: aName [

	^ self buildEnvironment at: aName ifAbsent: [ nil ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> classSlots [
	^ self layoutDefinition classSlots
]

{ #category : 'accessing' }
ShiftClassBuilder >> classSlots: aSlotCollection [
	self layoutDefinition classSlots: aSlotCollection
]

{ #category : 'accessing' }
ShiftClassBuilder >> comment [
	^ comment
]

{ #category : 'accessing' }
ShiftClassBuilder >> comment: anObject [
	comment := anObject
]

{ #category : 'accessing - comment' }
ShiftClassBuilder >> comment:aComment stamp: anStamp [
	self comment: aComment.
	self commentStamp: anStamp
]

{ #category : 'accessing' }
ShiftClassBuilder >> commentStamp [
	^ commentStamp
]

{ #category : 'accessing' }
ShiftClassBuilder >> commentStamp: anObject [
	commentStamp := anObject
]

{ #category : 'changes' }
ShiftClassBuilder >> compareWithOldClass [
	oldClass ifNil: [ ^ self ].
	changeComparers do: [ :e | e compareClass: oldClass with: self ].

	changes ifEmpty: [ ShNoChangesInClass signal. ]
]

{ #category : 'compiling' }
ShiftClassBuilder >> compileMethods [

	newClass
		compileAllFrom: self oldClass;
		removeNonexistentSelectorsFromProtocols
]

{ #category : 'building' }
ShiftClassBuilder >> copyProtocols [

	newClass protocols: oldClass protocols copy.
	newClass class protocols: oldClass class protocols copy
]

{ #category : 'installing' }
ShiftClassBuilder >> createClass [

	newClass := newMetaclass new.
	newClass
		setName: self name;
		basicCategory: Class unclassifiedCategory.

	self builderEnhancer
		configureClass: newClass
		superclass: self superclass
		withLayoutType: self layoutClass
		slots: (self withAdditionalSlots: self slots).

	newClass environment: self installingEnvironment.

	self builderEnhancer classCreated: self
]

{ #category : 'building' }
ShiftClassBuilder >> createMetaclass [

	newMetaclass := self builderEnhancer newMetaclass: self.

	self builderEnhancer
		configureMetaclass: newMetaclass
		superclass: self metaSuperclass
		withLayoutType: FixedLayout
		slots: (self withAdditionalSlots: self classSlots).

	self builderEnhancer metaclassCreated: self
]

{ #category : 'building' }
ShiftClassBuilder >> createSharedVariables [

	self builderEnhancer on: newClass declareClassVariables: self layoutDefinition sharedVariables sharing: self layoutDefinition sharedPools
]

{ #category : 'initialization' }
ShiftClassBuilder >> detectBuilderEnhancer [

	| builderEnhancerClass |

	builderEnhancerClass := ShDefaultBuilderEnhancer allSubclasses detect: [ :e | e isApplicableFor: self ] ifNone: [ self class defaultBuildEnhancer ].
	builderEnhancer := builderEnhancerClass new.

	builderEnhancer initializeBuilder: self.
	^ builderEnhancer
]

{ #category : 'reflective operations' }
ShiftClassBuilder >> doesNotUnderstand: aMessage [
	| selector variable setter|

	selector := aMessage selector.

	(selector isUnary or:[ selector isKeyword and:[ selector keywords size = 1] ])
		ifFalse:[ ^ super doesNotUnderstand: aMessage].

	setter := selector isKeyword.
	variable := setter ifTrue:[(selector allButLast: 1) asSymbol] ifFalse:[selector].

	(extensibleProperties includesKey: variable)
		ifFalse: [ ^ super doesNotUnderstand: aMessage ].

	setter
		ifTrue:[ extensibleProperties at: variable put: aMessage argument]
		ifFalse:[ ^ extensibleProperties at: variable]
]

{ #category : 'accessing' }
ShiftClassBuilder >> environment [

	^ self buildEnvironment
]

{ #category : 'accessing' }
ShiftClassBuilder >> environment: anObject [

	^ self buildEnvironment: anObject
]

{ #category : 'copying' }
ShiftClassBuilder >> fillClassSideFromEnvironment: anEnvironment [

	| old |
	old := anEnvironment at: name ifAbsent: [ ^ self ].

	self classSlots: old class slots.
	self classTraits: old class traitComposition
]

{ #category : 'initialization' }
ShiftClassBuilder >> fillFor: aClass [

	self flag: #package. "package tag should be improved in the future"
	self
		superclass: aClass superclass;
		name: aClass getName;
		layoutClass: aClass classLayout class;
		slots: aClass localSlots;
		classSlots: aClass class localSlots;
		sharedVariablesFromString: aClass classVariablesString;
		sharedPools: aClass sharedPoolsString;
		package: aClass package name;
		installingEnvironment: aClass environment;
		oldClass: aClass.

	aClass packageTag ifNotNil: [ :aTag | aTag isRoot ifFalse: [ self tag: aTag name ] ].

	self builderEnhancer fillBuilder: self from: aClass
]

{ #category : 'copying' }
ShiftClassBuilder >> fillInstanceSideFromClass: aClass [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	self privateSlots: aClass slots.
	self privateTraitComposition: aClass traitComposition.
	self package: aClass package name.

	aClass packageTag ifNotNil: [ :aTag | aTag isRoot ifFalse: [ self tag: aTag name ] ].

	self superclass: aClass superclass.

	self layout: aClass classLayout class.

	self sharedVariables: aClass classVariables.
	self sharedPools: (aClass sharedPools collect: [ :each | each name ]) asArray
]

{ #category : 'building' }
ShiftClassBuilder >> fluidInstall [
	"Install the class in the system environment, method #fluidInstall is implemented in behavior, too"

	^ self install
]

{ #category : 'testing' }
ShiftClassBuilder >> hasToMigrateInstances [
	^ self changes anySatisfy: [ :e | e hasToMigrateInstances ]
]

{ #category : 'initialization' }
ShiftClassBuilder >> initialize [

	super initialize.

	self useStrictSuperclass.
	superclassName := #Object.

	layoutDefinition := ShLayoutDefinition new.
	layoutDefinition builder: self.

	extensibleProperties := Dictionary new.

	changeComparers := OrderedCollection new.
	changes := Set new.

	package := 'Unclassified'.
	inRemake := false.

	buildEnvironment := self class environment
]

{ #category : 'actions' }
ShiftClassBuilder >> install [
	"Install the class in the system environment"

	^ ShiftClassInstaller new makeWithBuilder: self
]

{ #category : 'building' }
ShiftClassBuilder >> installSlotsAndVariables [
	"notify all the variables so they can react to being installed in a class"
	newClass classLayout slots do: [ :each | each installingIn: newClass ].
	newClass class classLayout slots do: [ :each | each installingIn: newClass class ].
	newClass classVariables do: [ :each | each installingIn: newClass ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> installingEnvironment [
	"The build environment is used to find the classes used during the building of a class such as the layouts, and the installing environment is the environment in which the class should be installed."

	^ installingEnvironment ifNil: [ self buildEnvironment ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> installingEnvironment: anObject [

	installingEnvironment := anObject
]

{ #category : 'accessing' }
ShiftClassBuilder >> isInRemake [

	"If the builder is in remake (when propagating changes to subclasses)"

	^ inRemake
]

{ #category : 'accessing' }
ShiftClassBuilder >> layout: aLayoutClass [
	"Added while merging FluidClassBuilder API and ShiftClassBuilder. Maybe we should deprecate #layoutClass:"

	^ self layoutClass: aLayoutClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> layoutClass [

	^ self layoutDefinition layoutClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> layoutClass: aLayoutClass [
	self layoutDefinition layoutClass: aLayoutClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> layoutDefinition [
	^ layoutDefinition
]

{ #category : 'accessing' }
ShiftClassBuilder >> markIsInRemake [

	inRemake := true
]

{ #category : 'accessing' }
ShiftClassBuilder >> metaSuperclass [

	^ metaSuperclass ifNil:[ (superclass ifNil: [ Class ] ifNotNil: [ superclass class ]) ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> metaSuperclass: aClass [

	metaSuperclass := aClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> metaclassClass [
	^ metaclassClass ifNil: [ Metaclass ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> metaclassClass: anObject [
	metaclassClass := anObject
]

{ #category : 'accessing' }
ShiftClassBuilder >> name [
	^ name
]

{ #category : 'accessing' }
ShiftClassBuilder >> name: anObject [
	name := anObject.
	self validateClassName
]

{ #category : 'accessing' }
ShiftClassBuilder >> newClass [
	^ newClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> newMetaclass [
	^ newMetaclass
]

{ #category : 'changes' }
ShiftClassBuilder >> notifyChanges [
	changes do: #announceChanges
]

{ #category : 'accessing' }
ShiftClassBuilder >> oldClass [
	^ oldClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> oldClass: anObject [
	oldClass := anObject.
	oldClass ifNotNil: [oldMetaclass := oldClass class]
]

{ #category : 'accessing' }
ShiftClassBuilder >> oldMetaclass [
	^ oldMetaclass
]

{ #category : 'accessing' }
ShiftClassBuilder >> package [

	^ package
]

{ #category : 'accessing' }
ShiftClassBuilder >> package: aString [
(#( #'Tool-Profilers-Tests' #'Morphic-Widgets-Tabs' #'Refactoring-Tests-Core' #'Ombu-Tests' #'Tool-DependencyAnalyser' #'AST-Core-Tests' #BaselineOfNewTools
	    #BaselineOfReflectionMirrors #'Reflectivity-Tests' #BaselineOfRing #'Keymapping-Settings' #'RPackage-Tests' #'MetacelloCommandLineHandler-Tests'
	    #BaselineOfFreeType #'Morphic-Widgets-FastTable-Tests' #'MetacelloCommandLineHandler-Core' #'Tools-CodeNavigation' #BaselineOfDisplay #'CodeImport-Traits'
	    #BaselineOfBeacon #'Spec2-Morphic' #'Calypso-SystemPlugins-InheritanceAnalysis-Browser' #'Fonts-Infrastructure-Tests' #'SortFunctions-Core'
	    #BaselineOfOSSubprocess #'Calypso-SystemPlugins-Reflectivity-Queries-Tests' #'Metacello-MC' #Sindarin #PharoSettings9 #'Iceberg-Plugin-Metacello'
	    #'Unicode-Tests-Character-Data' #BaselineOfParametrizedTests #'Settings-System' #'Slot-Examples' #'System-History-Tests' #'System-Identification-Tests'
	    #'Tool-Base' #'Calypso-SystemQueries-Tests-P2WithSubclassFromP1' #'Calypso-SystemPlugins-Traits-Queries-Tests'
	    #'Announcements-Core-Tests' #'Calypso-SystemPlugins-Traits-Queries' #'ReflectionMirrors-Primitives' #'Roassal-Animation' #'MonticelloTonel-Tests'
	    #'Metacello-Gitlab-Tests' #BeautifulComments #'System-CommandLineHandler' #'AST-Core-Traits' #BaselineOfFFICHeaderExtractor #'Commander2-Tests'
	    #'Tool-DependencyAnalyser-UI' #'SUnit-UI' #MonticelloRemoteRepositories #'JenkinsTools-ExtraReports' #'Math-Operations-Extensions-Tests'
	    #'ClassParser-Tests' #'Metacello-TestsMCCore' #BaselineOfSUnit #'Morphic-Widgets-ColorPicker' #'System-Sound' #FuzzyMatcher #ExternalProjectLoader
	    #'FreeType-Tests' #'Calypso-SystemQueries-Tests-P1WithHierarchy' #Microdown #'System-Support-Tests' #'Roassal-Inspector-Tests' #Commander2 #'Athens-Core-Tests'
	    #'Calypso-SystemTools-FullBrowser-Tests' #'System-OSEnvironments' #Instrumentation #'SortFunctions-Tests' #'NewTools-Morphic-Spotter'
	    #'DrTests-RottenGreenTestsFinder' #BaselineOfIceberg #BaselineOfReferenceFinder #'System-FileRegistry' #'Kernel-Tests' #'Jobs-Tests' #'Calypso-Browser-Tests'
	    #BaselineOfFuzzyMatcher #'WebBrowser-Core' #'Collections-Streams-Tests' #'Metacello-GitBasedRepository' #'Refactoring-Transformations-Tests'
	    #'Rubric-SpecFindReplaceDialog' #'Spec2-Code-Commands' #'Metacello-Reference' #'FFI-Kernel' #'Network-Mail-Tests' #'Flashback-Decompiler'
	    #'General-Rules' #'Spec2-Adapters-Morphic' #'System-CommandLine-TextSupport' #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
	    #'NewTools-Spotter' #'Collections-Arithmetic' #'Text-Edition' #'Calypso-SystemPlugins-Flags-Queries' #'Roassal-UML' #'Gofer-Core' #BaselineOfGeometry
	    #'System-Caching' #'Ring-Tests-Monticello' #'NECompletion-Preferences' #'FormCanvas-Core' #'Commander2-UI' #'Spec2-Code-Diff-Tests'
	    #'Renraku-Tests' #'SUnit-Rules-Tests' #'NewTools-Spotter-Processors-Tests' #'FlagBrowser-Spec-Pharo9' #BaselineOfFlashback #'ThreadedFFI-Graphics-Extensions'
	    #NautilusRefactoring #MenuRegistration #'Spec2-Code-Tests' #'TaskIt-Tests' #'NewTools-SpTextPresenterDecorators' #'ProfStef-Core' #'NewTools-Core'
	    #'FormCanvas-Tests' #'Zinc-Resource-Meta-Tests' #'Iceberg-Plugin-Migration' #BaselineOfOSWindow #'NewTools-Sindarin-Tools' #'DrTests-TestCoverage-Tests'
	    #'DrTests-CommentsToTests-Tests' #'System-Platforms' #BaselineOfCalypso #'System-Localization' #BaselineOfManifest #Hermes #'Kernel-Chronology-Extras'
	    #'Rubric-Styling' #ClassParser #'System-Identification' #ClassAnnotation #'Ring-ChunkImporter' #'Math-Operations-Extensions' #'Calypso-SystemPlugins-MethodDiffTool'
	    #'Morphic-Widgets-FastTable' #'ReflectionMirrors-Primitives-Tests' #'Calypso-SystemTools-QueryBrowser' #'System-Localization-Tests'
	    #BaselineOfReflectivity #'Calypso-SystemQueries-Tests' #'Roassal-Spec-Morphic' #'Kernel-Tests-Extended' #'Transcript-Core' #BaselineOfUnicode
	    #'ReferenceFinder-Core' #'UnifiedFFI-Tests' #'STON-Extensions' #'Tool-Registry' #'Metacello-Platform' #'Calypso-SystemTools-OldToolCompatibillity'
	    #'RottenTestsFinder-Tests' #'ThreadedFFI-UFFI-Overrides' #'FlagBrowser-Model' #'Manifest-Tests' #'NewTools-Debugger-Fuel' #'Athens-Balloon'
	    #'NewTools-DocumentBrowser' #BaselineOfFuel #BaselineOfNewToolsDocumentBrowser #'Spec2-Code-Morphic' #'Metacello-GitHub' #'Calypso-SystemPlugins-Reflectivity-Queries'
	    #'Ring-Definitions-Tests-Core' #'Calypso-SystemPlugins-Undeclared-Queries' #'NewTools-DocumentBrowser-GitHubResource'
	    #'Roassal-Chart' #BaselineOfDrTests #'Morphic-Widgets-TickList' #'Specific-Rules' #'Debugging-Utils-Tests' #'LibGit-FileSystem' #'Calypso-SystemPlugins-Critic-Queries-Tests'
	    #'RottenTestsFinder-FakeTests' #'Clap-Examples' #'Metacello-Base' #'Morphic-Core' #'Calypso-SystemPlugins-FFI-Queries' #'MonticelloFileTree-FileSystem-Utilities'
	    #'Network-Protocols' #'OSWindow-Core' #'Calypso-SystemPlugins-ClassScripts-Queries-Tests' #'Roassal-Event' #'Metacello-Bitbucket' #'SystemCommands-ClassCommands'
	    #'FileSystem-Tests-Attributes' #'Calypso-SystemPlugins-Deprecation-Queries-Tests' #'Roassal-Pie' #'SUnit-MockObjects' #'NewTools-Inspector-Tests'
	    #'Roassal-Global-Tests' #'Zinc-HTTP' #'OpalCompiler-Tests' #Hiedra #OSSubprocess #'Reflectivity-Examples' #'NewTools-Spotter-Extensions'
	    #'System-Settings-Browser' #PharoSettings8 #'Spec2-Adapters-Morphic-Tests' #'Transcript-NonInteractive-Tests' #'RPackage-Core' #'Spec2-Code' #StartupPreferences
	    #'NewTools-RewriterTools-Backend' #'System-Announcements-Tests' #'Metacello-ToolBox' #'Roassal-FlameGraph' #'Metacello-FileTree' #'MonticelloTonel-Tests-Mocks'
	    #'Spec2-Tests' #MonticelloMocks #BaselineOfMisc #FreeType #'Tool-ImageCleaner-Tests' #Deprecated12 #OldFileStream #ProfilerUI #'Athens-Cairo-Tests'
	    #'System-Model' #BaselineOfRoassalLayouts #BaselineOfEpicea #'Refactoring-Critics' #BaselineOfZodiac #'Tool-DependencyAnalyser-Tests-Data'
	    #NumericNoise #'Tool-Diff' #'Text-Scanning' #'Compression-Tests' #'Zodiac-Core' #'Geometry-Tests' #'UndefinedClasses-Tests' #'Unicode-Normalization'
	    #'Calypso-SystemPlugins-Flags-Browser' #'Text-Core' #NECompletion #BaselineOfMonticello #BaselineOfUnifiedFFI #BaselineOfHeuristicCompletion
	    #BaselineOfIDE #'Commander-Activators-Mouse' #'NewTools-Playground' #BaselineOfQA #'Spec2-Core' #'Collections-Unordered-Tests'
	    #'Metacello-Tutorial' #EmbeddedFreeType #'Spec2-Commander2-Tests' #'Tool-DependencyAnalyser-Tests' #'SystemCommands-SourceCodeCommands'
	    #'Roassal-Mondrian' #'FFICHeaderExtractor-LibC' #'Flashback-Decompiler-Tests' #'Collections-Weak' #'Morphic-Widgets-Taskbar-Tests'
	    #'Monticello-GUI-Diff' #'Commander-Activators-ContextMenu' #'Collections-Native' #'Files-Prompt' #'Sindarin-Tests' #'NewTools-Spotter-Processors'
	    #NumberParser #BaselineOfMicrodown #'Collections-Strings' #Reflectivity #'SystemCommands-MessageCommands' #BaselineOfUI #'Collections-Atomic-Tests'
	    #'Calypso-SystemPlugins-SUnit-Browser' #'Roassal-SVG' #ZeroConf #'Morphic-Widgets-Extra' #'Zinc-Character-Encoding-Core' #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
	    #'Kernel-ExtraUtils' #'Athens-Examples' #'Morphic-Examples' #BaselineOfDependencyAnalyzer #'Tools-Tests' #'NautilusRefactoring-Tests'
	    #'Zodiac-Extra' #_UnpackagedPackage #'Collections-Stack-Tests' #'System-OSEnvironments-Tests' #'NewTools-Debugger-Extensions'
	    #'NewTools-Debugger-Tests' #'DrTests-TestsProfiling-Tests' #'Metacello-TestsReference' #'Commander-Activators-TextView' #'Roassal-Shapes-Tests'
	    #PharoSettings12 #'System-Platforms-Tests' #'Spec2-Examples' #'Ring-Definitions-Containers' #'Graphics-Canvas' #'Calypso-SystemQueries-Tests-PExtendedByP1'
	    #'Calypso-SystemPlugins-FFI-Queries-Tests' #'Calypso-SystemPlugins-Traits-Browser' #'Roassal-DSM' #'Unicode-Character-Data' #'SystemCommands-MethodCommands'
	    #'Slot-Tests' #'DrTests-TestCoverage' #'Roassal-Layouts-Tests' #'Athens-SVG-PathConverter' #'NewTools-Sindarin-Commands-Tests'
	    #UndefinedClasses #BaselineOfTonel #'Roassal-Sunburst' #'Iceberg-Libgit-Filetree' #'Morphic-Widgets-Taskbar' #'Keymapping-KeyCombinations'
	    #'Transcript-NonInteractive' #'NewValueHolder-Core' #'Calypso-NavigationModel-Tests' #'System-Support' #BaselineOfZeroConf #'Graphics-Tests'
	    #'TraitsV2-Tests' #'Morphic-Widgets-Windows' #'AST-Core' #'Zinc-HTTP-Examples' #'System-DependenciesTests' #'System-BasicCommandLineHandler'
	    #BaselineOfSindarin #'System-Settings-Tests' #'EnlumineurFormatter-Tests' #'STON-Tests' #'System-SessionManager-Tests' #'Calypso-SystemPlugins-Monticello-Queries'
	    #'Tool-Profilers' #'Ring-Definitions-Monticello' #'CodeImport-Tests' #'Tool-Finder-Tests' #'Roassal-Builders' #'Iceberg-Metacello-Integration'
	    #'Equals-Tests' #'SUnit-Visitor' #'Spec2-Dialogs' #BaselineOfSpec2 #'Graphics-Primitives' #'NewTools-RewriterTools-Backend-Tests'
	    #'Morphic-Widgets-Scrolling' #'Settings-Graphics' #ReleaseTests #'Calypso-SystemPlugins-Undeclared-Browser' #'Calypso-NavigationModel'
	    #'Monticello-Tests' #'FileSystem-Core' #ThreadedFFI #BaselineOfEmergencyDebugger #'Commander2-UI-Tests' #'Roassal-BaselineMap' #BaselineOfSortFunctions
	    #'Morphic-Widgets-Pluggable' #'NewTools-Debugger-Commands' #BaselineOfShout #'Keymapping-Core' #'Graphics-Display Objects' #BaselineOfInstrumentation
	    #BaselineOfWelcomeBrowser #'FileSystem-Path' #'System-Installers-Tests' #'PharoSettings-Scripts' #'Athens-Text' #'Athens-Cairo' #'EmbeddedFreeType-Tests'
	    #'ConfigurationCommandLineHandler-Tests' #'Calypso-SystemPlugins-SUnit-Queries' #'SUnit-Core' #'Metacello-Gitlab' #'Random-Core' #'Collections-DoubleLinkedList'
	    #'Collections-Strings-Tests' #'Collections-DoubleLinkedList-Tests' #'Spec2-Code-Diff' #'Collections-Stack' #'Kernel-Traits' #'SUnit-Tests' #Coverage
	    #MonticelloGUI #'Images-Animated' #InitializePackagesCommandLineHandler #'Calypso-SystemPlugins-Reflectivity-Browser-Tests'
	    #'Graphics-Shapes' #'Tool-FileList' #'EpiceaBrowsers-Tests' #'Roassal-Spec-Tests' #'Zinc-Resource-Meta-Core' #'Tool-ImageCleaner' #'Epicea-Tests'
	    #'Graphics-Transformations' #BaselineOfMorphicCore #'Shift-Changes' #'Native-Browser' #'System-SourcesCondenser' #'Tool-Workspace' #'Roassal-Menu'
	    #EnlumineurFormatter #BaselineOfPharo #'Regex-Core-Tests' #BaselineOfKernelTests #'Spec2-Commands' #'CodeExport-Traits' #'SUnit-MockObjects-Tests'
	    #'Calypso-SystemPlugins-FileOut-Queries' #ParametrizedTests #'General-Rules-Tests' #BaselineOfRoassal #'Multilingual-Encodings' #CodeImportCommandLineHandlers
	    #'Clap-Tests' #'Refactoring-UI-Tests' #'Tool-Finder-UI' #'Zinc-Tests' #PharoSettings #'System-CommandLine' #'Tool-ProcessBrowser' #'Fuel-Core' #'Collections-Arithmetic-Tests'
	    #'Refactoring-Changes' #'Shift-Changes-Tests' #'Morphic-Widgets-List' #'SUnit-Support-UITesting-Tests' #Ombu #'Collections-Atomic' #'Morphic-Widgets-Tree'
	    #'OpalCompiler-Core' #PerformMessageCommandLineHandler #'Zinc-Character-Encoding-Tests' #'ThreadedFFI-Tests' #'Hiedra-Tests' #'System-History'
	    #'MonticelloFileTree-Core' #'SystemCommands-ClassCommands-Tests' #'FileSystem-Zip' #'System-Caching-Tests' #'Roassal-Interaction-Tests'
	    #'Collections-Abstract-Traits' #TaskIt #'UnifiedFFI-Legacy' #'Collections-Abstract-Tests' #TraitsV2 #'Random-Tests' #BaselineOfBaseLibraries
	    #'EmergencyDebugger-Tests' #'OSWindow-SDL2' #Epicea #'Commander-Core-Tests' #UnifiedFFI #Shout #'Calypso-SystemQueries-Tests-PWithSingleClass'
	    #'OSSubprocess-Tests-Unit' #'Calypso-SystemPlugins-Monticello-Browser' #'Transcript-Core-Traits' #BaselineOfNewValueHolder #Growl #BaselineOfProfilerUI
	    #'Spec2-Adapters-Stub' #'SUnit-Rules' #'NewTools-Debugger' #'Spec2-Code-Diff-Morphic' #'Network-Mail' #'NewTools-RewriterTools-Tests'
	    #'FreeType-Graphics' #BaselineOfClap #'ScriptingExtensions-Tests' #'System-Object Events-Tests' #'Ring-Tests-ChunkImporter' #'Debugger-Oups-Tests'
	    #'Calypso-SystemPlugins-ClassScripts-Queries' #'NewTools-Inspector' #'DrTests-TestCoverage-Tests-Mocks' #'NumberParser-Tests' #'Calypso-SystemQueries'
	    #'Calypso-SystemPlugins-Deprecation-Browser' #'Roassal-Shapes' #'NewTools-RewriterTools' #'Calypso-SystemPlugins-Traits-Queries-Tests-PWithTraits'
	    #'Collections-Tests' #'Roassal-Legend' #'PharoBootstrap-Initialization' #'Settings-Polymorph' #'Files-Tests' #'System-BasicCommandLineHandler-Tests'
	    #'System-Changes-Tests' #'Tool-Workspace-Tests' #BaselineOfClassAnnotation #'Shout-Tests' #'Roassal-Layouts-Util' #'Commander-Activators-Shortcut'
	    #'Shift-ClassInstaller-Tests' #'Refactoring-Tests-Environment' #'NewTools-Morphic' #'NewTools-SystemReporter' #'LibGit-Core' #'Tool-ExternalBrowser-Tests'
	    #'Collections-Native-Tests' #'SystemCommands-RefactoringSupport-Tests' #'Metacello-Core' #'Slot-Core' #'DrTests-TestsProfiling' #'Spec2-Layout'
	    #'Shift-ClassInstaller' #'Tool-Finder' #BaselineOfTraits #BaselineOfEnlumineurFormatter #'Beacon-Core-Tests' #'FreeType-Morphic' #'Fonts-Abstract'
	    #'System-Announcements' #PragmaCollector #BaselineOfAthens #'Morphic-Base' #CodeExport #'Manifest-Core' #RottenTestsFinder #'ThreadedFFI-UFFI'
	    #Monticello #'Morphic-Tests' #'Microdown-RichTextPresenter' #'Iceberg-Plugin' #'Collections-Abstract' #ScriptingExtensions #'Unicode-Casing'
	    #'Specific-Rules-Tests' #'Keymapping-Tools-Spec' #'ProfStef-Tests' #'Ring-RuntimeSupport' #'NewTools-CodeCritiques' #'Microdown-RichTextPresenter-Tests'
	    #FFICHeaderExtractor #'OSWindow-Tests' #'Polymorph-Widgets' #'Spec2-Morphic-Examples' #'Refactoring-Tests-Changes' #'Commander-Activators-WorldMenu'
	    #'NewTools-ObjectCentricBreakpoints' #'NewTools-CodeCritiques-Tests' #'Debugger-Model-Tests' #'Roassal-Class-Examples' #'Roassal-Animation-Tests'
	    #Iceberg #'Ring-Definitions-Tests-Containers' #'FileSystem-Memory' #'Fuel-Core-Tests' #'Gofer-Tests' #'Formatter-Settings' #'Shift-ClassBuilder'
	    #'Graphics-Fonts' #'Calypso-Ring' #UIManager #BaselineOfEquals #'System-Sources' #'Keymapping-Tests' #'Metacello-PharoExtensions'
	    #'Calypso-SystemTools-FullBrowser' #'NewTools-DocumentBrowser-Tests' #BaselineOfAIGraphAlgorithms #BaselineOfLibGit #'FileSystem-Tests-Disk'
	    #'Calypso-SystemPlugins-Deprecation-Queries' #EmergencyDebugger #PharoSettings11 #BaselineOfFlagBrowser #'Commander-Activators-DragAndDrop'
	    #'TraitsV2-Compatibility' #'Clap-Core' #'MonticelloTonel-FileSystem' #'Zinc-Zodiac-Tests' #'NewTools-Spotter-Tests' #'Roassal-Interaction'
	    #'NewTools-Sindarin-Commands' #BaselineOfExternalProjectLoader #Roassal #'Clap-Commands-Pharo' #'Clap-CommandLine' #'Ring-Definitions-Core'
	    #'Collections-Support' #'AI-Algorithms-Graph-Components' #BaselineOfBasicTools #'JenkinsTools-Core' #'SUnit-Support-UITesting' #'Hermes-Extensions'
	    #'System-Object Events' #'Regex-Core' #Equals #'Ring-Tests-Core' #'Unicode-Stream-Support' #'Unicode-Tests-Casing' #'Ring-Definitions-Tests-Monticello'
	    #'Roassal-Inspector' #'Commander-Spec2-Compatibility' #'Spec2-Transformations' #'Refactoring-Environment' #'Roassal-UML-Tests' #'Spec2-Backend-Tests'
	    #'Calypso-SystemTools-Core' #'Calypso-SystemPlugins-Critic-Queries' #'NewValueHolder-Tests' #'FuzzyMatcher-Tests' #'Tool-FileList-Tests'
	    #BaselineOfRefactoring #'AI-Algorithms-Graph-Tests' #'Spec2-Interactions' #'Refactoring-Core' #Tools #'Tool-ExternalBrowser' #'Refactoring-DataForTesting'
	    #'Beacon-Core' #'System-Hashing' #'NECompletion-Morphic' #'Refactoring-Tests-Critics' #'Collections-Sequenceable-Tests' #'ConfigurationCommandLineHandler-Core'
	    #'Collections-Support-Tests' #Renraku #BaselineOfCommander2 #'SystemCommands-PackageCommands' #'Calypso-SystemTools-QueryBrowser-Tests'
	    #'HeuristicCompletion-Model' #'Refactoring-UI' #'NewTools-Inspector-Extensions' #'NewTools-Debugger-Breakpoints-Tools'
	    #'DrTests-Tests' #'Collections-Unordered' #BaselineOfClassParser #'Athens-Core' #'Ring-TraitsV2Support' #'HeuristicCompletion-Tests'
	    #'Iceberg-TipUI' #'AI-Algorithms-Graph' #'Commander-Core' #'SystemCommands-RefactoringSupport' #'SystemCommands-MethodCommands-Tests'
	    #'System-SessionManager' #'Spec2-Code-Backend-Tests' #'Morphic-Widgets-Basic' #'Roassal-Spec' #'Iceberg-Libgit-Tonel' #BaselineOfPharoSettings
	    #'ZeroConf-Tests' #DrTests #'System-Hashing-Tests' #'Calypso-SystemPlugins-Flags-Queries-Tests' #'Calypso-SystemPlugins-Critic-Browser'
	    #'SUnit-Core-Traits' #'Ring-OldChunkImporter' #BaselineOfNumericScales #'Metacello-TestsCore' #'Athens-Morphic' #'Debugging-Core' #BaselineOfCommander
	    #'Collections-Weak-Tests' #'Ring-Monticello' #'Spec2-Commander2' #PharoDocComment #'PharoDocComment-Tests' #'Kernel-BytecodeEncoders'
	    #'VariablesLibrary-Tests' #BaselineOfSpecCore #'SUnit-Visitor-Tests' #'ClassAnnotation-Tests' #BaselineOfTaskIt #BaselineOfThreadedFFI
	    #'Zodiac-Tests' #'Calypso-SystemPlugins-FFI-Browser' #'NewTools-ChangeSorter' #NumericInterpolator #'Unicode-Tests-Normalization'
	    #'Spec2-CommonWidgets' #'Kernel-Tests-WithCompiler' #'Spec2-CommandLine' #EpiceaBrowsers #BaselineOfMenuRegistration #'Graphics-Files' #'Calypso-SystemQueries-Tests-P5WithTags'
	    #'Calypso-SystemPlugins-Undeclared-Queries-Tests' #'Network-UUID' #'FileSystem-Disk' #'Calypso-SystemPlugins-ClassScripts-Browser'
	    #BaselineOfGeneralTests #'Text-Tests' #BaselineOfTelePharoSupport #BaselineOfMorphic #'System-Finalization-Tests' #'Metacello-Cypress' #'Debugger-Oups'
	    #'MonticelloGUI-Tests' #'Roassal-Colors' #'Iceberg-Plugin-Pharo' #'MonticelloTonel-Core' #'Text-Diff' #BaselineOfShift #'Manifest-Resources-Tests'
	    #'Calypso-SystemPlugins-Traits-Queries-Tests-PExtendingTrait' #'Roassal-SVG-Tests' #'Iceberg-Plugin-GitHub' #'STON-Core' #'Iceberg-Libgit'
	    #'NewTools-DocumentBrowser-GitHubResource-Tests' #'Calypso-SystemPlugins-DependencyAnalyser-Browser' #'Debugger-Model' #'ThreadedFFI-UFFI-Tests'
	    #'NewTools-MethodBrowsers' #'NewTools-Debugger-Fuel-Tests' #'Microdown-RichTextComposer' #BaselineOfSystemCommands #'Spec2-Morphic-Tests'
	    #'NewTools-FontChooser-Tests' #CodeImport #BaselineOfMetacello #'Microdown-Tests' #'Calypso-SystemPlugins-FileOut-Browser'
	    #'NECompletion-Tests' #'System-Installers' #'Network-MIME' #'Text-Diff-Tests' #'Hiedra-Examples' #BaselineOfPharoBootstrap #'SystemCommands-VariableCommands'
	    #'Calypso-SystemPlugins-Reflectivity-Browser' #'NumericInterpolator-Tests' #'Roassal-BaselineMap-Tests' #BaselineOfQualityAssistant
	    #Files #'Calypso-Browser' #BaselineOfBeautifulComments #'DrTests-TestsRunner' #'FileSystem-Tests-Memory' #'Reflectivity-Tools' #'NewTools-Playground-Tests'
	    #'Spec2-Microdown' #'Network-Tests' #Geometry #BaselineOfSlot #'SystemCommands-PackageCommands-Tests' #'Network-Kernel' #'Calypso-SystemQueries-Tests-PExtendedByP5'
	    #'System-Settings-Core' #Kernel #'Shift-ClassBuilder-Tests' #'Spec2-Transmission' #Rubric #'Spec2-Morphic-Backend-Tests' #'NewTools-Fuel' #'FileSystem-Tests-Core'
	    #'Keymapping-Pragmas' #'System-Sources-Tests' #'System-Finalization' #'NewTools-WelcomeBrowser' #'OSWindow-SDL2-Examples' #'Tool-DependencyAnalyser-UI-Tab'
	    #'Roassal-Layouts' #'Reflectivity-Tools-Tests' #'Ring-Core' #'Announcements-Core' #BaselineOfKeymapping #'Rubric-Tests' #'Calypso-SystemPlugins-SUnit-Queries-Tests'
	    #'DrTests-CommentsToTests' #'Tools-CodeNavigation-Tests' #'FFICHeaderExtractor-Tests' #VariablesLibrary #'Roassal-Chart-Tests' #Colors #Jobs #'Collections-Sequenceable'
	    #'STON-Text support' #'Morphic-Widgets-PolyTabs' #Compression #'Transcript-Tool' #'System-Changes' #'Collections-Streams' #MonticelloConfigurations
	    #'Monticello-BackwardCompatibility' #'HeuristicCompletion-Morphic' #'NewTools-FontChooser' #MonticelloFileServices #'OSSubprocess-Tests-Stress'
	    #'System-Changes-FileServices' #'Refactoring-Transformations' #'Fonts-Infrastructure' #'Morphic-Widgets-Menubar' #'Calypso-SystemQueries-Tests-P3WithSubclassFromP2' )
		 includes: aString) ifFalse: [
		Stdio stdout
			nextPutAll: 'Tried to set a packages name ' , aString , ' in a Shift class builder. Stack:';
			lf;
			nextPutAll: thisContext longStack;
			lf;
			lf. self error. ].
	package := aString
]

{ #category : 'private' }
ShiftClassBuilder >> privateSlots: aCollection [
	self layoutDefinition slots: aCollection
]

{ #category : 'changes' }
ShiftClassBuilder >> propagateChangesTo: anotherBuilder [
	changes do: [ :e | e propagateToSubclasses: anotherBuilder ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> propertyAt: aKey [
	^ extensibleProperties at: aKey
]

{ #category : 'accessing' }
ShiftClassBuilder >> propertyAt: aKey put: aValue [
	extensibleProperties at: aKey put: aValue
]

{ #category : 'accessing' }
ShiftClassBuilder >> sharedPools [
	^ self layoutDefinition sharedPools
]

{ #category : 'accessing' }
ShiftClassBuilder >> sharedPools: anObject [
	"The paramter can be either a collection of symbols or a string that should be converted into a collection of symbols."

	self layoutDefinition sharedPools: (anObject isString
			 ifTrue: [ (anObject substrings: ' ') collect: [ :e | e asSymbol ] ]
			 ifFalse: [ anObject ])
]

{ #category : 'accessing' }
ShiftClassBuilder >> sharedVariables [
	^ self layoutDefinition sharedVariables
]

{ #category : 'accessing' }
ShiftClassBuilder >> sharedVariables: aCollection [
	self layoutDefinition sharedVariables: (aCollection collect:[:e | e asClassVariable])
]

{ #category : 'accessing' }
ShiftClassBuilder >> sharedVariablesFromString: aStringOrArray [
	layoutDefinition sharedVariables: (aStringOrArray isString
        ifTrue: [ (aStringOrArray substrings: ' ') collect: [ :x | x asSymbol => ClassVariable ] ]
        ifFalse: [ aStringOrArray collect: [ :x |
                x isSymbol
                    ifTrue: [ x => ClassVariable ]
                    ifFalse: [ self error: 'Shared variables can only be String or an array of Symbols' ] ] ])
]

{ #category : 'accessing' }
ShiftClassBuilder >> slots [
	^ self layoutDefinition slots
]

{ #category : 'accessing' }
ShiftClassBuilder >> slots: aCollection [

	self privateSlots: aCollection
]

{ #category : 'accessing' }
ShiftClassBuilder >> slotsFromString: aString [
	self slots: aString asSlotCollection
]

{ #category : 'accessing' }
ShiftClassBuilder >> superclass [

	^ superclass ifNil: [ superclassName ifNotNil: [ self classNamed: self superclassName ] ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> superclass: aSuperclass [

	superclass := aSuperclass.
	superclassName := superclass ifNotNil: [ superclass name ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> superclassName [
	^ superclassName
]

{ #category : 'accessing' }
ShiftClassBuilder >> superclassName: anObject [

	superclassName := anObject ifNotNil: [ anObject asSymbol ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> superclassResolver: asuperclassResolver [
	superclassResolver:= asuperclassResolver
]

{ #category : 'accessing' }
ShiftClassBuilder >> tag [

	^ tag
]

{ #category : 'accessing' }
ShiftClassBuilder >> tag: anObject [

	tag := anObject
]

{ #category : 'building' }
ShiftClassBuilder >> tryToFillOldClass [

	oldClass ifNotNil: [ ^ self ].
	name ifNil: [ ^ self ].
	self oldClass: (self classNamed: self name)
]

{ #category : 'building' }
ShiftClassBuilder >> useStrictSuperclass [
	"default behavior"
	"If I am use, the superResolver will resolve unknown superclass by raising an error"
	self superclassResolver: ClassResolverStrictResolve new
]

{ #category : 'private' }
ShiftClassBuilder >> validateClassName [
	name ifNil: [ ^self ].

	"I try to convert to symbol, if there is an error the next guard will catch it"
	[ name := name asSymbol ] on: Error do: [  ].

	name isSymbol ifFalse:[InvalidGlobalName
				signal: 'Global names should be symbols'
				for: name].

	name isValidGlobalName ifFalse: [
		InvalidGlobalName signal: 'Class name is not a valid global name. It must start with uppercase letter and continue with alphanumeric characters or underscore. Default names used in class or trait templates are not allowed.' for: name ].

	DangerousClassNotifier check: name
]

{ #category : 'private' }
ShiftClassBuilder >> validateSuperclass [
	self superclass ifNil: [ ^self ].
	oldClass ifNil: [ ^ self ].

	self superclass withAllSuperclassesDo: [ :aSuperclass |
		aSuperclass = oldClass ifTrue:[
			CircularHierarchyError signalFor: oldClass ]]
]

{ #category : 'private' }
ShiftClassBuilder >> withAdditionalSlots: aSlotCollection [
	"Some slots are relying on additional slots for internal behavior. This method adds them to the list of slots of the class."

	| slots |
	slots := aSlotCollection asOrderedCollection.

	aSlotCollection do: [ :slot | slot addAdditionalSlotsTo: slots ].

	^ slots asArray
]
