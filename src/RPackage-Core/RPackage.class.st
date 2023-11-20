"
A RPackage is a new implementation of package.


Instance Variables
	extensionSelectors:		Dictionary The list of extension selectors in the package mapped by classes
	name:		String is the name of the package
	classTags: Collection is the list of tags in the package
	organizer: PackageOrganizer is the organizer in which the package in included		
	
Implementation notes 
====================
This class went over 4 internal representations implementation. 
	- first: 	a list of class + two dictionaries: class * methods
	This was not good since we add to go all the time over the list of classes.
	- second: 4 dictionaries class * selectors 
	This was not good since we want to have defined classes without methods. 
	- third: 4 dictionaries + definedClasses
	This was not good because it was way too complex
	- Fourth: 1 dictionary + tags including the classes
	Now the classes are able to give their defining selectors really fast so there is no need to have the defining selectors. We just save a dictionary with the extended classes and the extension selectors for speed reasons.

Originally I wanted to avoid to have a defined class list and I wanted to avoid to have to declare the class as defined. But this is not really good since we will want to know if a class definition (without method for example) is defined in a given package

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
"
Class {
	#name : 'RPackage',
	#superclass : 'Object',
	#instVars : [
		'extensionSelectors',
		'name',
		'classTags',
		'organizer'
	],
	#classVars : [
		'Properties'
	],
	#category : 'RPackage-Core-Base',
	#package : 'RPackage-Core',
	#tag : 'Base'
}

{ #category : 'class initialization' }
RPackage class >> initialize [
	Properties
		ifNil: [ self initializeProperties ]
		ifNotNil: [
			| newDict |
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

{ #category : 'accessing' }
RPackage class >> rootTagName [

	^ #Uncategorized
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
		ifTrue: [ "We want to make sure the method is not considered an extension." aCompiledMethod removeProperty: #extensionPackage ]
		ifFalse: [
			aCompiledMethod propertyAt: #extensionPackage put: self. "We tag the method as an extension method."
			(extensionSelectors at: methodClass ifAbsentPut: [ IdentitySet new ]) add: aCompiledMethod selector ].

	^ aCompiledMethod
]

{ #category : 'class tags' }
RPackage >> classTagNamed: aSymbol [

	^ self classTags detect: [ :each | each name = aSymbol ]
]

{ #category : 'class tags' }
RPackage >> classTagNamed: aSymbol ifAbsent: aBlock [

	^ self classTags
		  detect: [ :each | each name = aSymbol ]
		  ifNone: aBlock
]

{ #category : 'class tags' }
RPackage >> classTagNamed: aSymbol ifPresent: aBlock [

	^ self classTags
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
RPackage >> codeChangeAnnouncer [

	^ self environment codeChangeAnnouncer
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

	^ (self includesClass: aClass)
		  ifTrue: [ aClass definedSelectors ]
		  ifFalse: [ #(  ) ]
]

{ #category : 'testing' }
RPackage >> definesOrExtendsClass: aClass [
	"Returns true whether the class, aClass, is one of the locally defined classes of the receiver or
	if the receiver extends such class (that is defined in another package)"

	^ (self includesClass: aClass instanceSide) or: [ self extendedClasses includes: aClass instanceSide ]
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

	newTag := PackageTag package: self name: tagName.
	classTags add: newTag.

	self codeChangeAnnouncer  announce: (PackageTagAdded to: newTag).
	^ newTag
]

{ #category : 'accessing' }
RPackage >> environment [

	^ self organizer environment
]

{ #category : 'accessing' }
RPackage >> extendedClassNames [
	"Return the name of the classes which are extended by the receiver package. if a metaclass is extended, just get its sole instance class name."

	^ self extendedClasses collect: [ :class | class instanceSide name ] as: Set
]

{ #category : 'accessing' }
RPackage >> extendedClasses [
	"Return classes and metaclasses that are extended in the receiver. They represent the classes of method extensions"

	^ (extensionSelectors keys collect: [ :class | class instanceSide ]) asSet
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

	"This check is here for speed reason. We first check that the package of the class is myself, and if it is, we also need to check that I contains the class because if the class is removed it will still know I am its package but I will not contain it anymore."
	aClass package = self ifFalse: [ ^ false ].
	
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

	^ ((self includesClass: aClass) and: [ aClass definedSelectors includes: aSelector ]) or: [ self includesExtensionSelector: aSelector ofClass: aClass ]
]

{ #category : 'initialization' }
RPackage >> initialize [

	super initialize.
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
	self definedClasses do: [ :class |
		methods
			addAll: class definedMethods;
			addAll: class classSide definedMethods ].

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
		self removeAllExtensionMethodsFromClass: aClass.
		
		{ aClass. aClass classSide } do: [ :class |
			(self extensionProtocolsForClass: class) do: [ :protocol | class renameProtocol: protocol as: Protocol unclassified ].

			class protocols
				reject: [ :protocol | protocol isExtensionProtocol ]
				thenDo: [ :protocol | self importProtocol: protocol forClass: class ] ] ].

	"If the tag is undefined, this means we are adding the class. Else we are updating the package or tag."
	oldTag isUndefined
		ifTrue: [ self codeChangeAnnouncer  announce: (ClassAdded class: aClass) ]
		ifFalse: [ self codeChangeAnnouncer  announce: (ClassRepackaged classRepackaged: aClass oldTag: oldTag newTag: newTag) ]
]

{ #category : 'accessing' }
RPackage >> name [

	^ name
]

{ #category : 'accessing' }
RPackage >> name: aSymbol [
	"Set the name of a package. This method is private and should not be used.
	If you wish to rename a package, use #renameTo: instead"

	aSymbol isEmptyOrNil ifTrue: [ self error: 'Cannot have empty or nil package name.' ].
	name := aSymbol asSymbol
]

{ #category : 'private' }
RPackage >> organizer [

	^ organizer
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
RPackage >> removeAllExtensionMethodsFromClass: aClass [
	"Remove all the methods (defined and extensions) that are related to the class as parameter. The class should always be instance side."

	extensionSelectors removeKey: aClass ifAbsent: [  ].
	extensionSelectors removeKey: aClass classSide ifAbsent: [  ]
]

{ #category : 'removing' }
RPackage >> removeClass: aClass [
	"I remove the class and potential empty tags from myself."

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
	"We need to act only on extension methods because defined methods are not stored anymore"
	(self includesClass: methodClass) ifFalse: [
		extensionSelectors at: methodClass ifPresent: [ :selectors |
			selectors remove: aCompiledMethod selector ifAbsent: [  ].
			selectors ifEmpty: [ extensionSelectors removeKey: methodClass ] ] ].

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

	self codeChangeAnnouncer  announce: (PackageTagRemoved to: tag)
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

	"We rename the extension protocols refering to this package"
	(self extensionMethods collect: [ :method | method protocol ] as: Set) do: [ :protocol |
		protocol rename: '*' , newName , (protocol name allButFirst: oldName size + 1) ].

	self organizer basicRegisterPackage: self.
	self codeChangeAnnouncer  announce: (PackageRenamed to: self oldName: oldName newName: newName)
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
	self definedClasses do: [ :class |
		allSelectors
			addAll: class definedSelectors;
			addAll: class classSide definedSelectors ].
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
