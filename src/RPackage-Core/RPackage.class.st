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

{ #category : 'constants' }
RPackage class >> defaultPackageName [
	^ #'_UnpackagedPackage'
]

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
	"TODO: deprecate system category / replace this with a direct call to moveClass:fromPackage:toTag:"

	"This does two things:
		- make sure the class tag for ourselves exists
		- change the class category as appropriate.
			(and by cascade, ensure systemClassRecategorizedAction: is called)."

	aClass instanceSide category: self rootTag categoryName
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
RPackage >> classTagCategoryNames [
	^ (Set with: self packageName), (self classTags collect: [:each | each categoryName])
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
RPackage >> definesClass: aClass [
	"Returns true if the class, aClass, is one of the locally defined classes"
	"should be probably removed since this is the same as includesClass: or the inverse"
	^ self includesClass: aClass
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
RPackage >> importClass: aClass inTag: aTag [
	"Import a class already created but not attached to a package to the receiver. It will import the class and its methods.

	Handle also *- convention. Methods defined in *category are not added to the package.
	
	If the class had an extension to us, then all methods in that extension are moved to 'as yet unclassified' and the extension protocol is deleted"

	"Question: should we check that for each extension, there is a real package behind or not?"

	self removeAllMethodsFromClass: aClass.
	
	(self ensureTag: aTag) addClass: aClass instanceSide.

	{ aClass . aClass classSide } do: [ :class |
		(self extensionProtocolsForClass: class) do: [ :protocol | class renameProtocol: protocol as: Protocol unclassified ].

		class protocols reject: [ :protocol | protocol isExtensionProtocol ] thenDo: [ :protocol | self importProtocol: protocol forClass: class ] ]
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
RPackage >> isDefault [
	"Return whether the receiver is the default package, i.e. the one used when nothing else is specified."
	^ self name = self class defaultPackageName
]

{ #category : 'testing' }
RPackage >> isDeprecated [
	^ self packageManifestOrNil
		ifNil: [ ^ false ]
		ifNotNil: [ :manifest | manifest isDeprecated ]
]

{ #category : 'testing' }
RPackage >> isEmpty [

	self name = self class defaultPackageName ifTrue: [ ^ false ].
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

	| oldPackage tag |
	tag := self ensureTag: aTag.
	aClass packageTag = tag ifTrue: [ ^ self ].

	oldPackage := aClass package.

	oldPackage removeClass: aClass.
	self importClass: aClass inTag: tag.

	SystemAnnouncer uniqueInstance classRepackaged: aClass from: oldPackage to: self
]

{ #category : 'accessing' }
RPackage >> name [

	^ name
]

{ #category : 'accessing' }
RPackage >> name: aSymbol [
	"Set the name of a package. This method is private and should not be used.
	If you wish to rename a package, use #renameTo: instead"
	name := aSymbol asSymbol
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

	^ name
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

	self isDefault ifTrue: [ ^ self ].
	self classTags do: [ :tag | tag removeFromSystem ].
	self extensionMethods do: [ :method | method removeFromSystem ].

	self organizer removePackage: self
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

	tag classes do: [ :class | class removeFromSystem ].

	classTags remove: tag ifAbsent: [ ^ self ].

	SystemAnnouncer announce: (PackageTagRemoved to: tag)
]

{ #category : 'private' }
RPackage >> renameExtensionsPrefixedWith: oldName to: newName [

	| protocols |
	protocols := self extensionMethods collect: [ :method | method protocol ] as: Set.

	protocols do: [ :protocol | protocol rename: '*' , newName , (protocol name allButFirst: oldName size + 1) ]
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

	| oldName newName oldCategoryNames |
	oldName := self name.
	newName := aSymbol asSymbol.
	self organizer validatePackageDoesNotExist: aSymbol.
	oldCategoryNames := (self classTags collect: [ :each | each categoryName ] as: Set)
		                    add: self name;
		                    difference: { newName }.
	self organizer basicUnregisterPackage: self.
	self name: aSymbol.
	SystemAnnouncer uniqueInstance suspendAllWhile: [ self definedClasses do: [ :each | each category: newName , (each category allButFirst: oldName size) ] ].
	self flag: #package. "For now, the root tag has the name of the package, thus, renaming the package means that we need to rename the root tag. In the future I want to update the root tag name to be fix and not depend on the name of the package. When that happens, we'll be able to remove the next line."
	self classTagNamed: oldName ifPresent: [ :tag | tag renameTo: newName ].
	self renameExtensionsPrefixedWith: oldName to: newName.
	self organizer basicRegisterPackage: self.
	SystemAnnouncer uniqueInstance announce: (PackageRenamed to: self oldName: oldName newName: newName)
]

{ #category : 'class tags' }
RPackage >> rootTag [

	^ self ensureTag: self rootTagName
]

{ #category : 'accessing' }
RPackage >> rootTagName [

	self flag: #package. "In the future we should not use the name of the package because it causes bugs."
	^ self name
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
		  detect: [ :tag | tag hasClass: aClass ]
		  ifNone: [ nil ]
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
