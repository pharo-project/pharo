"
A MCClassDefinition represents a class.
"
Class {
	#name : 'MCClassDefinition',
	#superclass : 'MCDefinition',
	#instVars : [
		'name',
		'superclassName',
		'variables',
		'packageName',
		'type',
		'comment',
		'commentStamp',
		'tagName',
		'traitComposition',
		'classTraitComposition'
	],
	#category : 'Monticello-Modeling',
	#package : 'Monticello',
	#tag : 'Modeling'
}

{ #category : 'deprecated' }
MCClassDefinition class >> name: nameString superclassName: superclassString traitComposition: traitCompositionString classTraitComposition: classTraitCompositionString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString commentStamp: stampString [
	"Do not use this method! This method will be deprecated once Tonel will not use it anymore."

	^ (self named: nameString)
		  category: categoryString;
		  superclassName: superclassString;
		  traitComposition: traitCompositionString;
		  classTraitComposition: classTraitCompositionString;
		  instVarNames: ivarArray;
		  classVarNames: cvarArray;
		  poolDictionaryNames: poolArray;
		  classInstVarNames: civarArray;
		  type: typeSymbol;
		  comment: commentString;
		  commentStamp: stampString;
		  yourself
]

{ #category : 'instance creation' }
MCClassDefinition class >> named: nameString [

	^ self new
		  name: nameString;
		  yourself
]

{ #category : 'comparing' }
MCClassDefinition >> = aDefinition [
	^ super = aDefinition
		and: [ superclassName = aDefinition superclassName
		and: [ self traitCompositionString = aDefinition traitCompositionString
		and: [ self classTraitCompositionString = aDefinition classTraitCompositionString
		and: [ self category = aDefinition category
		and: [ type = aDefinition type and: [ self sortedVariables = aDefinition sortedVariables and: [ comment = aDefinition comment ] ] ] ] ] ] ]
]

{ #category : 'visiting' }
MCClassDefinition >> accept: aVisitor [
	aVisitor visitClassDefinition: self.
	(self hasClassInstanceVariables or: [self hasClassTraitComposition])
		ifTrue: [aVisitor visitMetaclassDefinition: self].

]

{ #category : 'accessing' }
MCClassDefinition >> actualClass [
	^ Smalltalk globals classNamed: self className
]

{ #category : 'initialization' }
MCClassDefinition >> addVariables: aCollection ofType: aClass [

	variables addAll: (aCollection collect: [ :var | aClass name: var asString ])
]

{ #category : 'accessing' }
MCClassDefinition >> category [

	^ self tagName
		  ifNil: [ self packageName ]
		  ifNotNil: [ :tag | self packageName , '-' , tag ]
]

{ #category : 'accessing' }
MCClassDefinition >> category: categoryString [
	Stdio stdout
			nextPutAll: 'Should not use #category: to set ' , categoryString , '. Stack:';
			lf;
			nextPutAll: thisContext longStack;
			lf;
			lf.
	self error
	self packageName: categoryString
]

{ #category : 'accessing' }
MCClassDefinition >> classDefinitionString [
	"Answer a string describing the class-side definition."
	
	^String streamContents: [:stream | self printClassDefinitionOn: stream]
]

{ #category : 'accessing' }
MCClassDefinition >> classInstVarNames [
	^ self selectVariables: #isClassInstanceVariable
]

{ #category : 'accessing' }
MCClassDefinition >> classInstVarNames: civarArray [

	self addVariables: civarArray ofType: MCClassInstanceVariableDefinition
]

{ #category : 'printing' }
MCClassDefinition >> classInstanceVariables [

	variables ifNil: [ ^ #() ].

	^self needsSlotClassDefinition 
		ifTrue: [ self variablesOfType: #isClassInstanceVariable]
		ifFalse: [self classInstanceVariablesString asSlotCollection]
]

{ #category : 'printing' }
MCClassDefinition >> classInstanceVariablesString [

	^ self stringForVariablesOfType: #isClassInstanceVariable
]

{ #category : 'accessing' }
MCClassDefinition >> className [
	^ name
]

{ #category : 'accessing' }
MCClassDefinition >> classTraitComposition [
	^classTraitComposition
]

{ #category : 'accessing' }
MCClassDefinition >> classTraitComposition: classTraitCompositionString [

	classTraitComposition := classTraitCompositionString
]

{ #category : 'accessing' }
MCClassDefinition >> classTraitCompositionCompiled [
		^(Smalltalk compiler evaluate: self classTraitCompositionString) asTraitComposition 
]

{ #category : 'accessing' }
MCClassDefinition >> classTraitCompositionString [
	^self classTraitComposition ifNil: ['{}'].
]

{ #category : 'accessing' }
MCClassDefinition >> classVarNames [
	^(self selectVariables: #isClassVariable) asArray sort
]

{ #category : 'accessing' }
MCClassDefinition >> classVarNames: cvarArray [

	self addVariables: cvarArray asSortedCollection ofType: MCClassVariableDefinition
]

{ #category : 'printing' }
MCClassDefinition >> classVariables [
	^self needsSlotClassDefinition 
		ifTrue: [self sortedVariablesOfType: #isClassVariable ]
		ifFalse: [(self classVariablesString substrings: ' ') collect: [:x | x asSymbol => ClassVariable]].

]

{ #category : 'printing' }
MCClassDefinition >> classVariablesString [
	^ self stringForSortedVariablesOfType: #isClassVariable
]

{ #category : 'accessing' }
MCClassDefinition >> comment [
	^ comment
]

{ #category : 'accessing' }
MCClassDefinition >> comment: aString [

	comment := aString withInternalLineEndings
]

{ #category : 'accessing' }
MCClassDefinition >> commentStamp [
	^ commentStamp
]

{ #category : 'accessing' }
MCClassDefinition >> commentStamp: anObject [
	commentStamp := anObject
]

{ #category : 'installing' }
MCClassDefinition >> createClass [

	| superClass |
	"Ignore Context definition because of troubles with class migration on bootstrapped image. Temporary solution."
	name = #Context ifTrue: [
		Context comment = comment ifFalse: [ Context comment: comment stamp: self commentStamp ].
		^ self ].

	superClass := superclassName = #nil ifFalse: [ Smalltalk globals at: superclassName ].
	^ [
	  self class classInstaller make: [ :builder |
		  builder
			  superclass: superClass;
			  name: name;
			  layoutClass: (ObjectLayout layoutForType: type);
			  slots: self instanceVariables;
			  sharedVariables: self classVariables;
			  sharedPools: self sharedPoolsString;
			  classSlots: self classInstanceVariables;
			  traitComposition: self traitCompositionCompiled;
			  classTraitComposition: self classTraitCompositionCompiled;
			  comment: comment stamp: self commentStamp;
			  package: self packageName;
			  tag: self tagName;
			  environment: superClass environment ] ]
		  on: Warning , DuplicatedVariableError
		  do: [ :ex | ex resume ]
]

{ #category : 'installing' }
MCClassDefinition >> createVariableFromString: aString [
	^[ Smalltalk compiler evaluate: aString ] 
		on: Error 
		do: [ 
			"if an error happens, we parse the slot definition to an ast.
			and create a UndefinedSlot"
			| ast slotName |
			ast := RBParser parseExpression: aString. 
			slotName := ast receiver value. 
			UndefinedSlot named: slotName ast: ast  ]
]

{ #category : 'accessing' }
MCClassDefinition >> defaultCommentStamp [
	^ String new

	"The version below avoids stomping on stamps already in the image

	^ (Smalltalk globals at: name ifPresent: [:c | c organization commentStamp])
		ifNil: ['']
	"

]

{ #category : 'printing' }
MCClassDefinition >> definitionString [
	^ String streamContents: [:stream | self printDefinitionOn: stream]
]

{ #category : 'accessing' }
MCClassDefinition >> description [
	^ { name }

]

{ #category : 'testing' }
MCClassDefinition >> hasClassInstanceVariables [
	^ (self selectVariables: #isClassInstanceVariable) isEmpty not
]

{ #category : 'testing' }
MCClassDefinition >> hasClassTraitComposition [
	^self classTraitCompositionString ~= '{}'
]

{ #category : 'testing' }
MCClassDefinition >> hasComment [
	^ comment isEmptyOrNil not
]

{ #category : 'testing' }
MCClassDefinition >> hasTraitComposition [
	^self traitCompositionString ~= '{}'
]

{ #category : 'comparing' }
MCClassDefinition >> hash [

	^ (((((name hash bitXor: superclassName hash) bitXor: traitComposition hash) bitXor: classTraitComposition hash) bitXor: self category hash) bitXor: type hash)
		  bitXor: variables hash
]

{ #category : 'initialization' }
MCClassDefinition >> initialize [

	super initialize.
	superclassName := #Object.
	traitComposition := '{}'.
	classTraitComposition := '{}'.
	type := #normal.
	comment := ''.
	commentStamp := self defaultCommentStamp.
	variables := OrderedCollection new
]

{ #category : 'introspection' }
MCClassDefinition >> instVarNamed: aString put: aValue [

	self flag: #package. "temporary hack until Tonel is fixed"
	super
		instVarNamed: (aString = 'category'
				 ifTrue: [ 'packageName' ]
				 ifFalse: [ aString ])
		put: aValue
]

{ #category : 'accessing' }
MCClassDefinition >> instVarNames [
	^ self selectVariables: #isInstanceVariable
]

{ #category : 'accessing' }
MCClassDefinition >> instVarNames: ivarArray [

	self addVariables: ivarArray ofType: MCInstanceVariableDefinition
]

{ #category : 'printing' }
MCClassDefinition >> instanceVariables [
	^self needsSlotClassDefinition 
		ifTrue: [self variablesOfType: #isInstanceVariable]
		ifFalse: [self instanceVariablesString asSlotCollection]
]

{ #category : 'printing' }
MCClassDefinition >> instanceVariablesString [
	^ self stringForVariablesOfType: #isInstanceVariable
]

{ #category : 'testing' }
MCClassDefinition >> isClassDefinition [
	^ true
]

{ #category : 'testing' }
MCClassDefinition >> isCodeDefinition [
	^ true
]

{ #category : 'printing' }
MCClassDefinition >> kindOfSubclass [
	type = #normal ifTrue: [^ ' subclass: '].
	type = #words ifTrue: [^ ' variableWordSubclass: '].
	type = #variable ifTrue: [^ ' variableSubclass: '].
	type = #bytes ifTrue: [^ ' variableByteSubclass: '].
	type = #weak ifTrue: [^ ' weakSubclass: ' ].
	type = #compiledMethod ifTrue: [^ ' variableByteSubclass: ' ].
	type = #immediate ifTrue: [ ^ ' immediateSubclass: ' ].
	type = #ephemeron ifTrue: [ ^ ' ephemeronSubclass: ' ].
	type = #DoubleByteLayout ifTrue: [ ^' variableDoubleByteSubclass: ' ].
	type = #DoubleWordLayout ifTrue: [ ^' variableDoubleWordSubclass: ' ].
	"To support user defined layouts (load them as normal classes), we just return the default"
	^ ' subclass: '
]

{ #category : 'installing' }
MCClassDefinition >> load [
	self createClass
]

{ #category : 'accessing' }
MCClassDefinition >> name: anObject [

	name := anObject asSymbol.
	self type: type
]

{ #category : 'installing' }
MCClassDefinition >> needsSlotClassDefinition [
	"this checks if any ivar or class var is using more than just standard definitions.
	Complex vars are encoded with a string that starts with a # or one that has a space"
	
	^self variables anySatisfy: [:var | (var name beginsWith:'#') or: [ var name includes: Character space ]]
]

{ #category : 'accessing' }
MCClassDefinition >> packageName [

	^ packageName
]

{ #category : 'accessing' }
MCClassDefinition >> packageName: aString [
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
		 includes: name) ifFalse: [
		Stdio stdout
			nextPutAll: 'Tried to set a packages name ' , aString , ' in a MCClassDefinition. Stack:';
			lf;
			nextPutAll: thisContext longStack;
			lf;
			lf. self error. ].
	packageName := aString ifNotNil: [ :package | package asSymbol ]
]

{ #category : 'accessing' }
MCClassDefinition >> poolDictionaries [
	^ self selectVariables: #isPoolImport
]

{ #category : 'accessing' }
MCClassDefinition >> poolDictionaryNames: poolArray [

	self addVariables: poolArray asSortedCollection ofType: MCPoolImportDefinition
]

{ #category : 'copying' }
MCClassDefinition >> postCopy [

	super postCopy.
	variables := variables copy
]

{ #category : 'annotations' }
MCClassDefinition >> printAnnotations: requests on: aStream [
	"Add a string for an annotation pane, trying to fulfill the browser annotationRequests"

	requests do: [ :aRequest |
		aRequest == #requirements ifTrue: [
			self requirements do: [ :req | aStream nextPutAll: req ] separatedBy: [ aStream space ]]
	] separatedBy: [ aStream space ].
]

{ #category : 'printing' }
MCClassDefinition >> printClassDefinitionOn: stream [
	"Print a class-side definition of the receiver on the given stream.
	Class instance variables and class traits."
	
		stream
			nextPutAll: self className;
			nextPutAll: ' class';
			cr; tab.
		self hasClassTraitComposition ifTrue: [
			stream 
				nextPutAll: 'uses: ';
		 		nextPutAll: self classTraitCompositionString;
				cr; tab ].
		stream
			nextPutAll: 'instanceVariableNames: ';
			store: self classInstanceVariablesString
]

{ #category : 'printing' }
MCClassDefinition >> printDefinitionBodyOn: stream [

	self hasClassTraitComposition ifTrue: [
		stream
			nextPutAll: 'uses: ';
			nextPutAll: self classTraitCompositionString;
			cr; tab ].
	stream
		nextPutAll: ' instanceVariableNames: '; 
		store: self classInstanceVariablesString
]

{ #category : 'printing' }
MCClassDefinition >> printDefinitionOn: stream [
		stream 
			nextPutAll: self superclassName;
			nextPutAll: self kindOfSubclass;
			nextPut: $# ;
			nextPutAll: self className;
			cr; tab.
		self hasTraitComposition ifTrue: [
			stream 
				nextPutAll: 'uses: ';
		 		nextPutAll: self traitCompositionString;
				cr; tab ].
		stream
			nextPutAll: 'instanceVariableNames: ';
			store: self instanceVariablesString;
			cr; tab;
			nextPutAll: 'classVariableNames: ';
			store: self classVariablesString;
			cr; tab;
			nextPutAll: 'poolDictionaries: ';
			store: self sharedPoolsString;
			cr; tab;
			nextPutAll: 'category: ';
			store: self category asString
]

{ #category : 'printing' }
MCClassDefinition >> printMetaDefinitionOn: stream [
	stream 
		nextPutAll: self className;
		nextPutAll: ' class'; cr; tab.
	self printDefinitionBodyOn: stream
]

{ #category : 'comparing' }
MCClassDefinition >> provisions [
	^ { name }
]

{ #category : 'comparing' }
MCClassDefinition >> requirements [

	^ (superclassName = #nil or: [ superclassName asString beginsWith: 'AnObsolete' ])
		  ifTrue: [ self poolDictionaries ]
		  ifFalse: [ { superclassName } , self poolDictionaries ]
]

{ #category : 'accessing' }
MCClassDefinition >> selectVariables: aSelector [
	^ variables select: [:v | v perform: aSelector] thenCollect: [:v | v name]
]

{ #category : 'printing' }
MCClassDefinition >> sharedPoolsString [
	^ self stringForSortedVariablesOfType: #isPoolImport
]

{ #category : 'accessing' }
MCClassDefinition >> sortKey [
	^ self className
]

{ #category : 'accessing' }
MCClassDefinition >> sortedVariables [
	"sort variables for comparison purposes"

	| sorted |
	sorted := variables select: [:var | var isOrderDependend].
	sorted addAll: ((variables reject: [:var | var isOrderDependend])
		asSortedCollection: [:a :b | a name <= b name]).
	^sorted
]

{ #category : 'installing' }
MCClassDefinition >> sortedVariablesOfType: aSymbol [
	"version for complex vars, { definition . definition }"
	^(((self selectVariables: aSymbol) asSortedCollection collect: [:each | Smalltalk compiler evaluate: each])) asArray.
		
]

{ #category : 'printing' }
MCClassDefinition >> source [
	^ self definitionString
]

{ #category : 'storing' }
MCClassDefinition >> storeDataOn: aDataStream [
	| instVarSize |
	instVarSize := (self hasTraitComposition or: [ self hasClassTraitComposition ])
		ifTrue: [ self class instSize ]
		ifFalse: [ self class instSize - 2 ].
	aDataStream
		beginInstance: self class
		size: instVarSize.
	1 to: instVarSize do: [ :index |
		aDataStream nextPut: (self instVarAt: index) ].
]

{ #category : 'installing' }
MCClassDefinition >> stringForSortedVariablesOfType: aSymbol [
	^ String streamContents:
		[:stream |
		(self selectVariables: aSymbol) asSortedCollection
			do: [:ea | stream nextPutAll: ea]
			separatedBy: [stream space]]
]

{ #category : 'installing' }
MCClassDefinition >> stringForVariablesOfType: aSymbol [
	^ String streamContents:
		[:stream |
		(self selectVariables: aSymbol) 
			do: [:ea | stream nextPutAll: ea]
			separatedBy: [stream space]]
]

{ #category : 'printing' }
MCClassDefinition >> summary [
	^ name
]

{ #category : 'accessing' }
MCClassDefinition >> superclassName [
	^ superclassName
]

{ #category : 'accessing' }
MCClassDefinition >> superclassName: superclassString [

	superclassName := superclassString
		                  ifNil: [ #nil ]
		                  ifNotNil: [ superclassString asSymbol ]
]

{ #category : 'accessing' }
MCClassDefinition >> tagName [

	^ tagName
]

{ #category : 'accessing' }
MCClassDefinition >> tagName: aString [

	tagName := aString = RPackage rootTagName
		           ifTrue: [ nil ]
		           ifFalse: [ aString asSymbol ]
]

{ #category : 'accessing' }
MCClassDefinition >> traitComposition [
	^traitComposition
]

{ #category : 'accessing' }
MCClassDefinition >> traitComposition: traitCompositionString [

	traitComposition := traitCompositionString
]

{ #category : 'accessing' }
MCClassDefinition >> traitCompositionCompiled [
	^(Smalltalk compiler evaluate: self traitCompositionString) asTraitComposition 
]

{ #category : 'accessing' }
MCClassDefinition >> traitCompositionString [
	^self traitComposition ifNil: ['{}'].
]

{ #category : 'accessing' }
MCClassDefinition >> type [
	^ type
]

{ #category : 'accessing' }
MCClassDefinition >> type: aSymbol [

	type := (#( #CompiledMethod #CompiledBlock #CompiledCode ) includes: name)
		        ifTrue: [ #compiledMethod ]
		        ifFalse: [ aSymbol ]
]

{ #category : 'installing' }
MCClassDefinition >> unload [
	Smalltalk globals removeClassNamed: name
]

{ #category : 'accessing' }
MCClassDefinition >> variables [
	^ variables ifNil: [ variables := OrderedCollection  new ]
]

{ #category : 'installing' }
MCClassDefinition >> variablesOfType: aSymbol [
	"version for complex vars, { definition . definition }"
	^(self selectVariables: aSymbol) 
		collect: [:each | self createVariableFromString: each]
		as: Array
]
