"
A RPackage is a new implementation of package.


Instance Variables
	definedSelectors:		Dictionary The list of defined selectors in the package mapped by classes (excludes the extension selectors)
	extensionSelectors:		Dictionary The list of extension selectors in the package mapped by classes
	name:		String is the name of the package
	classTags: Collection is the list of tags in the package
	organizer: RPackageOrganizer is the organizer in which the package in included		
	
Implementation notes 
====================
This class went over 4 internal representations implementation. 
	- first: 	a list of class + two dictionaries: class * methods
	This was not good since we add to go all the time over the list of classes.
	- second: 4 dictionaries class * selectors 
	This was not good since we want to have defined classes without methods. 
	- third: 4 dictionaries + definedClasses
	This was not good because it was way too complex
	- Fourth: 2 dictionaries + tags including the classes
	The is the idea of the first iteration but the code got cleaned and it is now way easier to implement

Originally I wanted to avoid to have a defined class list and I wanted to avoid to have to declare the class as defined. But this is not really good since
	- we will want to know if a class definition (without method for example is defined in a given package)
	- second this is easier to hook the packageOf: behavior (we register the package class when the class is 	added to the package).

defined classes information is redundant with the dictionary keys of defined methods but we would have to check and register the class to the packageOrganizer the first time a method is defined then also check on remove to unregister the class from the packageOrganizer.

Adding a method does not define the class as a defined package class. This has to be done explictly. The reason for this choice is that a class should register to the packageOrganizer and that I do not want to do it each time a method is added and I do not want to test it each time a method is added. Now this is done only when the class is declared as defined. 
We could also give the complete freedom to the client to register the class but I thought it was a good compromise. 
	
	
ClassTag are tags that can be associated to classes. They help user organizing their class internal. 
So that we can have a package infrastructure as follows:
	Package1
		ClassA
		ClassB
	in case there is no tags associated to the package
	or
	Package2	
		Tag1
			ClassA
			ClassB
		Tag2
			ClassC
			ClassD
			ClassE
			ClassA		
	
Todo
====
	Next: 
	- finish 
"
Class {
	#name : 'RPackage',
	#superclass : 'Object',
	#instVars : [
		'extensionSelectors',
		'definedSelectors',
		'name',
		'classTags',
		'organizer'
	],
	#classVars : [
		'PackageGlobalOrganizer',
		'Properties'
	],
	#category : 'RPackage-Core-Base',
	#package : 'RPackage-Core',
	#tag : 'Base'
}

{ #category : 'class initialization' }
RPackage class >> initialize [
	"Ensure the organizer will be the RPackageOrganizer default"
	self organizer: nil.
	Properties
		ifNil: [ self initializeProperties ]
		ifNotNil: [ | newDict |
			newDict := WeakIdentityKeyDictionary newFrom: Properties.
			newDict rehash.
			Properties := newDict ]
]

{ #category : 'class initialization' }
RPackage class >> initializeProperties [
	Properties := WeakIdentityKeyDictionary new
]

{ #category : 'instance creation' }
RPackage class >> named: aString [

	^ self new
		  name: aString;
		  yourself
]

{ #category : 'instance creation' }
RPackage class >> named: aString organizer: aPackageOrganizer [

	^ (self named: aString)
		  organizer: aPackageOrganizer;
		  yourself
]

{ #category : 'protected only for tests' }
RPackage class >> organizer [

	^ PackageGlobalOrganizer ifNil: [ self packageOrganizer ]
]

{ #category : 'protected only for tests' }
RPackage class >> organizer: aPackageOrganizer [

	PackageGlobalOrganizer := aPackageOrganizer
]

{ #category : 'accessing' }
RPackage class >> rootTagName [

	^ #Uncategorized
]

{ #category : 'protected only for tests' }
RPackage class >> withOrganizer: aNewOrganizer do: aBlock [
	"Perform an action locally to aNewOrganizer. Does not impact any other organizers."

	| old |
	[ old := self organizer.
	old unregister.
	self organizer: aNewOrganizer.
	aNewOrganizer register.
	aBlock cull: aNewOrganizer] ensure: [
		self organizer: old.
		old register.
		aNewOrganizer unregister]
]

{ #category : 'comparing' }
RPackage >> <= aRPackage [

	^ self name <= aRPackage name
]

{ #category : 'add class' }
RPackage >> addClass: aClass [
	"If no tag is given we put the class in the root tag"

	self moveClass: aClass toTag: self rootTag
]

{ #category : 'add method - compiled method' }
RPackage >> addMethod: aCompiledMethod [
	"Add the method to the receiver as a defined method if the class is  defined in it, else as an extension."

	| methodClass |
	methodClass := aCompiledMethod methodClass.
	(self includesClass: methodClass)
		ifTrue: [ (definedSelectors at: methodClass ifAbsentPut: [ IdentitySet new ]) add: aCompiledMethod selector ]
		ifFalse: [
			(extensionSelectors at: methodClass ifAbsentPut: [ IdentitySet new ]) add: aCompiledMethod selector.
			"we added a method extension so the receiver is an extending package of the class"
			self organizer registerExtendingPackage: self forClass: methodClass ].

	^ aCompiledMethod
]

{ #category : 'class tags' }
RPackage >> classTagNamed: aSymbol [

	^ classTags detect: [ :each | each name = aSymbol ]
]

{ #category : 'class tags' }
RPackage >> classTagNamed: aSymbol ifAbsent: aBlock [
	^ classTags
		detect: [ :each | each name = aSymbol ]
		ifNone: aBlock
]

{ #category : 'class tags' }
RPackage >> classTagNamed: aSymbol ifPresent: aBlock [
	^ classTags
		detect: [ :each | each name = aSymbol ]
		ifFound: [ :tag | aBlock cull: tag ]
		ifNone: [ nil ]
]

{ #category : 'class tags' }
RPackage >> classTags [
	"Returns the tags of the receiver"

	^ classTags
]

{ #category : 'accessing' }
RPackage >> classes [
	"Return all the classes"
	^ self definedClasses, self extendedClasses
]

{ #category : 'class tags' }
RPackage >> classesTaggedWith: aSymbol [
	"Returns the classes tagged using aSymbol"

	^ (self classTagNamed: aSymbol ifAbsent: [ ^ #(  ) ]) classes
]

{ #category : 'accessing' }
RPackage >> definedClassNames [
	"Return the class names having methods defined in the receiver."

	^ self definedClasses collect: [ :class | class name ]
]

{ #category : 'accessing' }
RPackage >> definedClasses [

	^ self classTags flatCollect: [ :tag | tag classes ]
]

{ #category : 'accessing' }
RPackage >> definedMethodsForClass: aClass [

	^ (self definedSelectorsForClass: aClass) asOrderedCollection collect: [ :each | aClass >> each ]
]

{ #category : 'accessing' }
RPackage >> definedOrExtendedClasses [
	^ self definedClasses | self extendedClasses
]

{ #category : 'accessing' }
RPackage >> definedSelectorsForClass: aClass [

	^ definedSelectors at: aClass ifAbsent: [ #(  ) ]
]

{ #category : 'testing' }
RPackage >> definesOrExtendsClass: aClass [
	"Returns true whether the class, aClass, is one of the locally defined classes of the receiver or
	if the receiver extends such class (that is defined in another package)"

	| canonizedName |
	canonizedName := aClass instanceSide name.
	^ (self includesClassNamed: canonizedName) or: [ self extendedClassNames includes: canonizedName ]
]

{ #category : 'converting' }
RPackage >> demoteToTagInPackage [

	| newPackage tag |
	(self name includes: $-) ifFalse: [ self error: 'To demote a package, it name needs to contain at least one dash `-`.' ].

	newPackage := self organizer ensurePackage: (self name copyUpToLast: $-).

	"We keep the suffix that was removed as the tag name to create."
	tag := newPackage ensureTag: (self name withoutPrefix: newPackage name , '-').

	self definedClasses do: [ :class | newPackage moveClass: class toTag: tag ].
	self extensionMethods do: [ :method |
		newPackage addMethod: method.
		self removeMethod: method ].

	self removeFromSystem.

	^ newPackage
]

{ #category : 'properties' }
RPackage >> ensureProperties [
	^ Properties at: self ifAbsentPut: WeakKeyDictionary new
]

{ #category : 'class tags' }
RPackage >> ensureTag: aTag [

	| tagName newTag |
	aTag ifNil: [ ^ self rootTag ].
	tagName := aTag isString
		           ifTrue: [ aTag ]
		           ifFalse: [ aTag name ].

	(self hasTag: aTag) ifTrue: [ ^ self classTagNamed: tagName ].

	newTag := RPackageTag package: self name: tagName.
	classTags add: newTag.

	SystemAnnouncer announce: (PackageTagAdded to: newTag).
	^ newTag
]

{ #category : 'accessing' }
RPackage >> environment [

	^ self organizer environment
]

{ #category : 'accessing' }
RPackage >> extendedClassNames [
	"Return the name of the classes which are extended by the receiver package. if a metaclass is extended, just get its sole instance class name."

	^ extensionSelectors keys collect: [ :class | class instanceSide name ] as: Set
]

{ #category : 'accessing' }
RPackage >> extendedClasses [
	"Return classes and metaclasses that are extended in the receiver. They represent the classes of method extensions"

	^ extensionSelectors keys
]

{ #category : 'testing' }
RPackage >> extendsClass: aClass [
	"Returns true if the receiver extends aClass (that is defined in another package)"

	| canonizedName |
	canonizedName := aClass instanceSide name.
	^ self extendedClassNames includes: canonizedName
]

{ #category : 'accessing' }
RPackage >> extensionMethods [
	"Extension methods are methods defined on classes that are not defined in the receiver"

	| methods |
	methods := OrderedCollection new.
	extensionSelectors keysAndValuesDo: [ :class :selectors | methods addAll: (selectors collect: [ :selector | class >> selector ]) ].
	^ methods
]

{ #category : 'accessing' }
RPackage >> extensionMethodsForClass: aClass [
	"Change the set of extensions selectors to an Array to avoid compiled methods collisions in the resulting set."

	^ (self extensionSelectorsForClass: aClass) asArray collect: [ :each | aClass >> each ]
]

{ #category : 'system compatibility' }
RPackage >> extensionProtocolsForClass: aClass [

	^ aClass protocols select: [ :protocol | protocol isExtensionProtocolMatching: self ]
]

{ #category : 'accessing' }
RPackage >> extensionSelectors [
	"Extension methods are methods defined on classes that are not defined in the receiver"

	| allSelectors |
	allSelectors := Set new.
	extensionSelectors keysAndValuesDo: [ :class :selectors | allSelectors addAll: selectors ].
	^ allSelectors
]

{ #category : 'accessing' }
RPackage >> extensionSelectorsForClass: aClass [

	^ extensionSelectors at: aClass ifAbsent: [ #(  ) ]
]

{ #category : 'properties' }
RPackage >> hasProperty: aKey [
	self propertyAt: aKey ifAbsent: [ ^ false ].
	^ true
]

{ #category : 'testing' }
RPackage >> hasTag: aTag [
	"Takes a package tag or a package tag name as parameter and return true if I include this package tag."

	^ self tagNames includes: (aTag isString
			   ifTrue: [ aTag ]
			   ifFalse: [ aTag name ])
]

{ #category : 'accessing' }
RPackage >> hierarchyRoots [
	"Returns all the hiearchy roots (by opposition to single classes inheriting from class outside the package) of a package"

	^ self definedClasses
		select: [ :each | (each superclass isNil or: [ each superclass package ~~ self ]) and: [ each hasSubclasses ] ]
]

{ #category : 'private' }
RPackage >> importProtocol: aProtocol forClass: aClass [
	"import all the local methods of a protocol as defined in the receiver."

	(aClass methodsInProtocol: aProtocol)
		reject: [ :method | method isFromTrait ]
		thenDo: [ :method | self addMethod: method ]
]

{ #category : 'testing' }
RPackage >> includesClass: aClass [
	"Returns true if the receiver includes aClass in the classes that are defined within it: only class definition are considered - not class extensions"

	^ self classTags anySatisfy: [ :tag | tag includesClass: aClass ]
]

{ #category : 'testing' }
RPackage >> includesClassNamed: aSymbol [
	"Returns true if the receiver includes class named aSymbol in the classes that are defined within it: only class definition are considered - not class extensions"

	^ self definedClasses anySatisfy: [ :class | class name = aSymbol ]
]

{ #category : 'testing' }
RPackage >> includesClassTagNamed: aString [
	^ self classTags anySatisfy: [ :each | each name = aString ]
]

{ #category : 'testing' }
RPackage >> includesClassesAffectedBy: aSystemAnnouncement [

	^(aSystemAnnouncement affectsClassesDefinedInPackage: self)
		or: [ aSystemAnnouncement affectsClassesExtendedInPackage: self ]
]

{ #category : 'testing' }
RPackage >> includesDefinedSelector: aSelector ofClass: aClass [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined in this package"

	^ (self definedSelectorsForClass: aClass) includes: aSelector asSymbol
]

{ #category : 'testing' }
RPackage >> includesExtensionSelector: aSelector ofClass: aClass [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods extending other packages"

	^ (self extensionSelectorsForClass: aClass) includes: aSelector asSymbol
]

{ #category : 'testing' }
RPackage >> includesMethodsAffectedBy: aSystemAnnouncement [

	^aSystemAnnouncement affectsMethodsDefinedInPackage: self
]

{ #category : 'system compatibility' }
RPackage >> includesProtocol: protocol ofClass: aClass [

	(protocol isExtensionProtocolMatching: self) ifTrue: [ ^ true ].


	^ (self includesClass: aClass) and: [ protocol isExtensionProtocol not ]
]

{ #category : 'testing' }
RPackage >> includesSelector: aSelector ofClass: aClass [
	"Return true if the receiver includes the method of selector aSelector. Checks methods defined locally as well as extending other packages"

	^ (self includesDefinedSelector: aSelector ofClass: aClass) or: [ self includesExtensionSelector: aSelector ofClass: aClass ]
]

{ #category : 'initialization' }
RPackage >> initialize [

	super initialize.
	definedSelectors := IdentityDictionary new.
	extensionSelectors := IdentityDictionary new.
	classTags := Set new
]

{ #category : 'testing' }
RPackage >> isDeprecated [
	^ self packageManifestOrNil
		ifNil: [ ^ false ]
		ifNotNil: [ :manifest | manifest isDeprecated ]
]

{ #category : 'testing' }
RPackage >> isEmpty [

	^ self classes isEmpty and: [ self extensionSelectors isEmpty ]
]

{ #category : 'testing' }
RPackage >> isTestPackage [
	"1. Test package ends with suffix -Tests. Suffix is case sensitive.
	 2. Or test package contains '-Tests-' in middle.
	Some examples: "

	"(RPackage named: 'MockPackage-Tests') isTestPackage >>> true"
	"(RPackage named: 'MockPackage-tests') isTestPackage >>> true"
	"(RPackage named: 'MockPackage') isTestPackage >>> false"
	"(RPackage named: 'MockPackage-Tests-Package') isTestPackage >>> true"

	^ (self name endsWith: '-Tests' caseSensitive: false) or: [ self name includesSubstring: '-Tests-' caseSensitive: false ]
]

{ #category : 'testing' }
RPackage >> isUndefined [

	^ false
]

{ #category : 'accessing' }
RPackage >> linesOfCode [
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."
	^self methods inject: 0 into: [:sum :each | sum + each linesOfCode]
]

{ #category : 'accessing' }
RPackage >> methods [
	"Return all the methods defined in this package. Including extension methods (i.e., methods defined on a class that is not defined by me)"

	| methods |
	methods := OrderedCollection new.

	extensionSelectors keysAndValuesDo: [ :class :selectors | methods addAll: (selectors collect: [ :selector | class >> selector ]) ].
	definedSelectors keysAndValuesDo: [ :class :selectors | methods addAll: (selectors collect: [ :selector | class >> selector ]) ].

	^ methods
]

{ #category : 'accessing' }
RPackage >> methodsForClass: aClass [
	"Returns all compiled methods for a class in the receiver without distinction of extensions or not"

	^ (self includesClass: aClass)
		ifFalse: [self extensionMethodsForClass: aClass]
		ifTrue: [self definedMethodsForClass: aClass]
]

{ #category : 'private' }
RPackage >> moveClass: aClass toTag: aTag [
	"I am the main method to add a class to myself. 
	I will take care of removing the class from its old package. I'll renamed its extensions protocols refering myself into the unclassified protocol. I'll add the class to the right tag and announce the change."

	| oldPackage newTag oldTag |
	newTag := self ensureTag: aTag.
	oldTag := aClass packageTag.
	oldTag = newTag ifTrue: [ ^ self ].

	oldPackage := aClass package.

	"It is possible that we are just updating the tag and not the package.
	If we update the package we should remove the class from the old one and update the methods"
	oldPackage = self ifTrue: [ oldTag removeClass: aClass ] ifFalse: [ oldPackage removeClass: aClass ].

	newTag privateAddClass: aClass instanceSide.
	
	oldPackage = self ifFalse: [
		"In case I contain extension methods."
		self removeAllMethodsFromClass: aClass.
		
		{ aClass. aClass classSide } do: [ :class |
			(self extensionProtocolsForClass: class) do: [ :protocol | class renameProtocol: protocol as: Protocol unclassified ].

			class protocols
				reject: [ :protocol | protocol isExtensionProtocol ]
				thenDo: [ :protocol | self importProtocol: protocol forClass: class ] ] ].

	"If the tag is undefined, this means we are adding the class. Else we are updating the package or tag."
	oldTag isUndefined
		ifTrue: [ SystemAnnouncer uniqueInstance announce: (ClassAdded class: aClass) ]
		ifFalse: [ SystemAnnouncer uniqueInstance announce: (ClassRepackaged classRepackaged: aClass oldTag: oldTag newTag: newTag) ]
]

{ #category : 'accessing' }
RPackage >> name [

	^ name
]

{ #category : 'accessing' }
RPackage >> name: aSymbol [
	"Set the name of a package. This method is private and should not be used.
	If you wish to rename a package, use #renameTo: instead"

	name := aSymbol asSymbol.
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
			nextPutAll: 'Tried to create a packages name ' , name , '. Stack:';
			lf;
			nextPutAll: thisContext longStack;
			lf;
			lf. self error. ]
]

{ #category : 'private' }
RPackage >> organizer [

	self flag: #package.
	^ organizer ifNil: [ self class organizer ]
]

{ #category : 'accessing' }
RPackage >> organizer: anObject [

	organizer := anObject
]

{ #category : 'accessing' }
RPackage >> packageManifestOrNil [
	^ self definedClasses
		detect: [ :each | each isManifest ]
		ifNone: [ nil ]
]

{ #category : 'accessing' }
RPackage >> packageName [

	self deprecated: 'Use #name instead.' transformWith: '`@rcv packageName' -> '`@rcv name'.
	^ self name
]

{ #category : 'system compatibility' }
RPackage >> packages [
	"Compatibility with monticello and old PackageInfo"
	^ self classTags
]

{ #category : 'printing' }
RPackage >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: self name.
	aStream nextPut: $)
]

{ #category : 'properties' }
RPackage >> properties [
	^ Properties at: self ifAbsent: nil
]

{ #category : 'properties' }
RPackage >> propertyAt: propName [
	^ self
		propertyAt: propName
		ifAbsent: [ nil ]
]

{ #category : 'properties' }
RPackage >> propertyAt: propName ifAbsent: aBlock [
	self properties ifNil: [^aBlock value].
	^ self properties
		at: propName
		ifAbsent: aBlock
]

{ #category : 'properties' }
RPackage >> propertyAt: aKey ifAbsentPut: aBlock [
	"Answer the property associated with aKey or, if aKey isn't found store the result of evaluating aBlock as new value."

	^ self propertyAt: aKey ifAbsent: [ self propertyAt: aKey put: aBlock value ]
]

{ #category : 'properties' }
RPackage >> propertyAt: propName put: propValue [
	^ self ensureProperties
		at: propName
		put: propValue
]

{ #category : 'private - register' }
RPackage >> removeAllMethodsFromClass: aClass [
	"Remove all the methods (defined and extensions) that are related to the class as parameter. The class should always be instance side."

	definedSelectors removeKey: aClass ifAbsent: [  ].
	definedSelectors removeKey: aClass classSide ifAbsent: [  ].
	extensionSelectors removeKey: aClass ifAbsent: [  ].
	extensionSelectors removeKey: aClass classSide ifAbsent: [  ].

	self organizer unregisterExtendingPackage: self forClass: aClass
]

{ #category : 'removing' }
RPackage >> removeClass: aClass [
	"I remove the class, methods and potential empty tags from myself."

	"First I remove all the methods of the class from myself."
	self removeAllMethodsFromClass: aClass.

	"Then I unregister it from the tags"
	self classTags
		detect: [ :tag | tag includesClass: aClass ]
		ifFound: [ :tag | tag removeClass: aClass ]
]

{ #category : 'class tags' }
RPackage >> removeEmptyTags [

	(self classTags select: [ :tag | tag isEmpty ]) do: [ :emptyTag | self removeTag: emptyTag ]
]

{ #category : 'removing' }
RPackage >> removeFromSystem [
	"Copy to not remove collection over which we are iterating."

	self classTags copy do: [ :tag | tag removeFromSystem ].
	self extensionMethods do: [ :method | method removeFromSystem ].

	self organizer unregisterPackage: self
]

{ #category : 'add method - compiled method' }
RPackage >> removeMethod: aCompiledMethod [
	"Remove the method to the receiver as a defined method."

	| methodClass |
	methodClass := aCompiledMethod methodClass.
	(self includesClass: methodClass)
		ifTrue: [
			definedSelectors at: methodClass ifPresent: [ :selectors |
				selectors remove: aCompiledMethod selector ifAbsent: [  ].
				selectors ifEmpty: [ definedSelectors removeKey: methodClass ] ] ]
		ifFalse: [
			extensionSelectors at: methodClass ifPresent: [ :selectors |
				selectors remove: aCompiledMethod selector ifAbsent: [  ].
				selectors ifEmpty: [ extensionSelectors removeKey: methodClass ] ] ].

	((extensionSelectors at: methodClass instanceSide ifAbsent: [ #(  ) ]) isEmpty and: [
		 (extensionSelectors at: methodClass classSide ifAbsent: [ #(  ) ]) isEmpty ]) ifTrue: [
		self organizer unregisterExtendingPackage: self forClass: methodClass ].

	^ aCompiledMethod
]

{ #category : 'add method - compiled method' }
RPackage >> removeMethods: aCollection [
	aCollection do: [ :each | self removeMethod: each ]
]

{ #category : 'properties' }
RPackage >> removePropertiesIfEmpty [
	^ Properties at: self ifPresent: [ :dict |
		dict ifEmpty: [ Properties removeKey: self ] ]
]

{ #category : 'properties' }
RPackage >> removeProperty: propName [
	^ self
		removeProperty: propName
		ifAbsent: [ nil ]
]

{ #category : 'properties' }
RPackage >> removeProperty: propName ifAbsent: aBlock [
	| property |
	self properties ifNil: [^aBlock value].
	property := self properties
		removeKey: propName
		ifAbsent: aBlock.
	self removePropertiesIfEmpty.
	^ property
]

{ #category : 'class tags' }
RPackage >> removeTag: aTag [

	| tag |
	tag := aTag isString
		       ifTrue: [ self classTagNamed: aTag ifAbsent: [ ^ self ] ]
		       ifFalse: [ aTag ].

	"The #asArray is there to not remove elements from the #classes inst var while iterating it."
	tag classes asArray do: [ :class | class removeFromSystem ].

	classTags remove: tag ifAbsent: [ ^ self ].

	SystemAnnouncer announce: (PackageTagRemoved to: tag)
]

{ #category : 'class tags' }
RPackage >> renameTag: aTag to: newName [

	(self hasTag: aTag) ifFalse: [ ^ self ].

	(aTag isString
		 ifTrue: [ self classTagNamed: aTag ]
		 ifFalse: [ aTag ]) renameTo: newName
]

{ #category : 'register' }
RPackage >> renameTo: aSymbol [
	"Rename a package with a different name, provided as a symbol"

	| oldName newName |
	oldName := self name.
	newName := aSymbol asSymbol.
	self organizer validatePackageDoesNotExist: aSymbol.

	self organizer basicUnregisterPackage: self.
	self name: aSymbol.

	self flag: #package. "For now, the root tag has the name of the package, thus, renaming the package means that we need to rename the root tag. In the future I want to update the root tag name to be fix and not depend on the name of the package. When that happens, we'll be able to remove the next line."
	self classTagNamed: oldName ifPresent: [ :tag | tag renameTo: newName ].

	self flag: #package. "The next line is necessary until classes know their tag. Then it will not be needed anymore"
	self classTags do: [ :tag | tag classes do: [ :class | class basicCategory: tag categoryName ] ].

	"We rename the extension protocols refering to this package"
	(self extensionMethods collect: [ :method | method protocol ] as: Set) do: [ :protocol |
		protocol rename: '*' , newName , (protocol name allButFirst: oldName size + 1) ].

	self organizer basicRegisterPackage: self.
	SystemAnnouncer uniqueInstance announce: (PackageRenamed to: self oldName: oldName newName: newName)
]

{ #category : 'class tags' }
RPackage >> rootTag [

	^ self ensureTag: self rootTagName
]

{ #category : 'accessing' }
RPackage >> rootTagName [

	^ self class rootTagName
]

{ #category : 'accessing' }
RPackage >> roots [
	"Returns all the root classes of a package. A root class is a class whose superclass is not defined in the package.
	Root classes are potentially root of inheritance trees defined in a package."

	^ self definedClasses
		select: [ :each | each superclass isNil or: [ each superclass package ~~ self ] ]
]

{ #category : 'accessing' }
RPackage >> selectors [

	| allSelectors |
	allSelectors := Set new.
	extensionSelectors valuesDo: [ :selectors | allSelectors addAll: selectors ].
	definedSelectors valuesDo: [ :selectors | allSelectors addAll: selectors ].
	^ allSelectors
]

{ #category : 'accessing' }
RPackage >> selectorsForClass: aClass [
	"Returns all selectors for a class in the receiver without distinction of extensions or not"

	^ (self includesClass: aClass)
		ifFalse: [self extensionSelectorsForClass: aClass]
		ifTrue: [self definedSelectorsForClass: aClass]
]

{ #category : 'class tags' }
RPackage >> tagNames [

	^ self classTags collect: [ :tag | tag name ]
]

{ #category : 'class tags' }
RPackage >> tagOf: aClass [

	^ self classTags
		  detect: [ :tag | tag includesClass: aClass ]
		  ifNone: [
			  self error: ('No tag containing {1} found in package {2}' format: {
						   aClass name.
						   self name }) ]
]

{ #category : 'private' }
RPackage >> toTagName: aSymbol [

	^ (aSymbol beginsWith: self name asString , '-')
		  ifTrue: [ (aSymbol allButFirst: self name size + 1) asSymbol ]
		  ifFalse: [
			  (aSymbol sameAs: self name)
				  ifTrue: [ self rootTagName ]
				  ifFalse: [ aSymbol ] ]
]
