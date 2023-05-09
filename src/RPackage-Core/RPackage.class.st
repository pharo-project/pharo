"
A RPackage is a new implementation of package. Note that it does not touch classes but it is just a layer on top of classes therefore removing a method from a package does not change the underlying class. 


Instance Variables
	classDefinedSelectors:		Dictionary
	classExtensionSelectors:		Dictionary
	metaclassDefinedSelectors:		Dictionary
	metaclassExtensionSelectors:		Dictionary
	packageName:		String
	classes: OrderedCollection of Symbols
	classTags: is a mapping from tags to classNames

The reason we use four dictionaries is that this way we only store #Point in both as the class and the metaclass are involved. We do not have to with Point_class problem when comparing defined and extended classes. The idea is taken from the RBSelectorEnvironment. 					
		
Class Invariant one:
	Classes should not contain metaclass name, but only class names
	
	

Implementation notes 
====================
This class went over 3 internal representations implementation. 
	- first: 	a list of class + two dictionaries: class * methods
	This was not good since we add to go all the time over the list of classes.
	- second: 4 dictionaries class * selectors 
	This was not good since we want to have defined classes without methods. 
	- third: 4 dictionaries + definedClasses

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
	- build up a synchronizer that import PackageInfo.
	

	

	

"
Class {
	#name : #RPackage,
	#superclass : #Object,
	#instVars : [
		'classDefinedSelectors',
		'metaclassDefinedSelectors',
		'classExtensionSelectors',
		'metaclassExtensionSelectors',
		'classes',
		'name',
		'classTags'
	],
	#classVars : [
		'PackageGlobalOrganizer',
		'Properties'
	],
	#category : #'RPackage-Core-Base'
}

{ #category : #constants }
RPackage class >> defaultPackageName [
	^ #'_UnpackagedPackage'
]

{ #category : #'class initialization' }
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

{ #category : #'class initialization' }
RPackage class >> initializeProperties [
	Properties := WeakIdentityKeyDictionary new
]

{ #category : #'instance creation' }
RPackage class >> named: aString [

	^ self new
		  name: aString;
		  yourself
]

{ #category : #'protected only for tests' }
RPackage class >> organizer [

	^ PackageGlobalOrganizer ifNil: [ Smalltalk organization ]
]

{ #category : #'protected only for tests' }
RPackage class >> organizer: aPackageOrganizer [

	PackageGlobalOrganizer := aPackageOrganizer
]

{ #category : #'class initialization' }
RPackage class >> packageOrganizerClass [
	^ RPackageOrganizer
]

{ #category : #'protected only for tests' }
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

{ #category : #comparing }
RPackage >> <= aRPackage [

	^ self name <= aRPackage name
]

{ #category : #'class tags' }
RPackage >> actualClassTags [

	(classTags size = 1 and: [classTags anyOne isRoot]) ifTrue: [ ^#() ].

	^classTags
]

{ #category : #'add class' }
RPackage >> addClass: aClass [
	"TODO: deprecate system category / replace this with a direct call to moveClass:fromPackage:toTag:"

	"This does two things:
		- make sure the class tag for ourselves exists
		- change the class category as appropriate.
			(and by cascade, ensure systemClassRecategorizedAction: is called)."

	aClass category: (self addClassTag: self name) categoryName
]

{ #category : #'add class' }
RPackage >> addClassDefinition: aClass [
	"Add only the class definition of aClass, as a class defined by the receiver. The term classdefinition denotes the fact that we do not treat its methods. aClass methods are not added to the package. aClass can be either a class or a metaclass"

	self addClassDefinitionName: aClass instanceSide name
]

{ #category : #'class tags' }
RPackage >> addClassDefinition: aClass toClassTag: aSymbol [
	"Tags the class aClass with the tag aSymbol"

	self addClassDefinitionName: aClass name toClassTag: aSymbol
]

{ #category : #'add class' }
RPackage >> addClassDefinitionName: aClassName [
	"aClassName should only be a class (and not a metaclass name)"

	| aClassNameSymbol |
	('* class' match: aClassName)
		ifTrue: [ ^ self error: 'no metaclass name' ].
	aClassNameSymbol := aClassName asSymbol.
	(classes includes: aClassNameSymbol)
		ifTrue: [ ^ self ].
	classes add: aClassNameSymbol.
	self extendedMethodsShouldBecomeDefinedWhenAddingClassName: aClassNameSymbol.
	self registerClassName: aClassNameSymbol
]

{ #category : #'class tags' }
RPackage >> addClassDefinitionName: aClassName toClassTag: aSymbol [
	"Tags the class aClass with the tag aSymbol"

	(self addClassTag: aSymbol) addClassNamed: aClassName
]

{ #category : #'class tags' }
RPackage >> addClassTag: aSymbol [
	"Add the class tag from the receiver, if already added do nothing."
	| tagName newTag |

	"strip package name if needed"
	tagName := self toTagName: aSymbol.

	^ self
		classTagNamed: tagName
		ifAbsent: [
			self class organizer
				validateCanBeAddedPackage: self
				tagName: tagName.
			newTag := self basicAddClassTag: tagName.
			SystemAnnouncer uniqueInstance classTagAdded: tagName inPackage: self.
			^newTag]
]

{ #category : #'add method - compiled method' }
RPackage >> addMethod: aCompiledMethod [
	"Add the method to the receiver as a defined method if the class is  defined in it, else as an extension."
	| methodClass |

	methodClass := aCompiledMethod methodClass.
	(self includesClass: methodClass )
		ifTrue: [
			methodClass isMeta
				ifTrue: [
					(metaclassDefinedSelectors
						at: methodClass instanceSide name
						ifAbsentPut: [ Set new ])
						add: aCompiledMethod selector]
				ifFalse: [
					(classDefinedSelectors
						at: methodClass name
						ifAbsentPut: [ Set new ])
						add: aCompiledMethod selector ] ]
		ifFalse:  [
			methodClass isMeta
				ifTrue: [
					(metaclassExtensionSelectors
						at: methodClass instanceSide name
						ifAbsentPut: [ Set new ])
						add: aCompiledMethod selector.]
				ifFalse: [
					(classExtensionSelectors
						at: methodClass name
						ifAbsentPut: [ Set new ])
						add: aCompiledMethod selector].
				"we added a method extension so the receiver is an extending package of the class"
				self organizer registerExtendingPackage: self forClass: methodClass ].

	^ aCompiledMethod
]

{ #category : #'add method - selector' }
RPackage >> addSelector: aSelector ofClassName: aClassName [
	"Add the method to the receiver. If the class is not locally defined in that package then the method is defined as a method extension: ie extending another package. Note that this method does not add the method to the class, it just records in the package that the method is packaged."

	( self includesClassNamed: aClassName)
		ifFalse: [(classExtensionSelectors at: aClassName asSymbol ifAbsentPut: [Set new]) add: aSelector]
		ifTrue: [(classDefinedSelectors at: aClassName asSymbol ifAbsentPut: [Set new]) add: aSelector].
	^ aSelector
]

{ #category : #'add method - selector' }
RPackage >> addSelector: aSelector ofMetaclassName: aClassName [
	"Add the method to the receiver. If the class is not locally defined in that package then the method is defined as a method extension: ie extending another package. Note that this method does not add the method to the class, it just records in the package that the method is packaged. aClassName is the sole instance class name and not its metaclass one: i.e. adding Point class>>new is done as addSelector: #new ofMetaclassName: #Point"

	( self includesClassNamed: aClassName)
		ifFalse: [(metaclassExtensionSelectors at: aClassName asSymbol ifAbsentPut: [Set new]) add: aSelector]
		ifTrue: [(metaclassDefinedSelectors at: aClassName asSymbol ifAbsentPut: [Set new]) add: aSelector].
	^ aSelector
]

{ #category : #private }
RPackage >> basicAddClassTag: tagName [
	| packageTag |

	packageTag := RPackageTag package: self name: tagName.
	classTags add: packageTag.
	packageTag ensureSystemCategory.

	^ packageTag
]

{ #category : #private }
RPackage >> basicImportClass: aClass [
	"Add the class definition and all the selectors which are not extensions of other packages. If the class had an extension to us, then all methods in that extension are moved to 'as yet unclassified' and the extension protocol is deleted (should I use silently or not?)."

	"Question: should we check that for each extension, there is a real package behind or not?"

	| protocols |
	self addClassDefinition: aClass.
	aClass organization protocolNames do: [ :each |
		(self isYourClassExtension: each) ifTrue: [ aClass organization renameProtocol: each as: Protocol unclassified ] ].
	protocols := aClass organization protocolNames reject: [ :each | each first = $* ].
	protocols do: [ :each | self importProtocol: each forClass: aClass ]
]

{ #category : #'add method - selector' }
RPackage >> basicRemoveSelector: aSelector ofClassName: aClassName [
	"Remove the method in the package but does not propagate to the class itself.  Note that this method does not remove the method from the class, it just records in the package that the method is not in the package anymore."

	| selectors |

	selectors := ( self includesClassNamed: aClassName)
		ifFalse: [(classExtensionSelectors  at: aClassName ifAbsent: [ ^ Set new])]
		ifTrue: [(classDefinedSelectors  at: aClassName ifAbsent: [ ^ Set new])].

	selectors remove: aSelector ifAbsent: [ ]
]

{ #category : #'add method - selector' }
RPackage >> basicRemoveSelector: aSelector ofMetaclassName: aClassName [
	"Remove the method in the package. Note that this method does not remove the method from the class, it just records in the package that the method is not in the package anymore. aClassName is the sole instance class name and not its metaclass one: i.e. adding Point class>>new is done as removeSelector: #new ofMetaclassName: #Point"

	| selectors |
	selectors := ( self includesClassNamed: aClassName)
		ifFalse: [(metaclassExtensionSelectors  at: aClassName ifAbsent: [ ^ Set new])]
		ifTrue: [(metaclassDefinedSelectors  at: aClassName ifAbsent: [ ^ Set new])].

	selectors remove: aSelector ifAbsent: [  ]
]

{ #category : #'class tags' }
RPackage >> basicRemoveTag: tag [
	classTags remove: tag ifAbsent: [].

	SystemAnnouncer uniqueInstance
		classTagRemoved: tag name fromPackage: self
]

{ #category : #'system compatibility' }
RPackage >> category: categoryName matches: prefix [
	"self default category: 'Compression-Archives' matches: 'Compression'"
	"self default category: 'Compression' matches: 'Compression-Archives'"
	"self default category: 'Compression' matches: 'Compression'"
	"self default category: 'Compression-Arh' matches: 'Compression'"
	| prefixSize catSize |
	categoryName ifNil: [ ^false ].
	catSize := categoryName size.
	prefixSize := prefix size.
	catSize < prefixSize ifTrue: [ ^false ].
	(categoryName findString: prefix startingAt: 1 caseSensitive: false) = 1
		ifFalse: [ ^false ].
	^(categoryName at: prefix size + 1 ifAbsent: [ ^true ]) = $-
]

{ #category : #slices }
RPackage >> classDefinedSlicesDo: aBlock [
	"This method iterates over the defined class and their associated selectors. a slice is a class * list of selectors. aBlock will be applied to all the extensions slices of the receiver. aBlok first argument is the class and the second argument a list of method selectors"

	classDefinedSelectors keysAndValuesDo: aBlock
]

{ #category : #private }
RPackage >> classExtensionSelectors [
	^ classExtensionSelectors
]

{ #category : #slices }
RPackage >> classExtensionSlicesDo: aBlock [
	"This method iterates over the class extensions and their associated selectors. A slice is a class * list of selectors. aBlock will be applied to all the extensions slices of the receiver. aBlock first argument is the class and the second argument a list of method selectors"

	classExtensionSelectors keysAndValuesDo: aBlock
]

{ #category : #slices }
RPackage >> classNamesAndExtensionSelectorsDo: aBlock [
	"Iterate over the extended methods grouped by classes and apply the argument.
	The block will be passed a class name and each individual selectors.
	Only classes with methods are paired with their methods"

	classExtensionSelectors keysAndValuesDo: [ :classSymbol :methods |
			methods do: [ :sel |  aBlock value: classSymbol	value: sel]].

	metaclassExtensionSelectors keysAndValuesDo: [ :classSymbol :methods |
			methods do: [ :sel |  aBlock
									value: (classSymbol, '_class') asSymbol
									value: sel]]
]

{ #category : #'class tags' }
RPackage >> classNamesForClassTag: aSymbol [
	"Returns the classes tagged using aSymbol"

	^ (self
		classTagNamed: aSymbol
		ifAbsent: [ ^ #() ]) classNames
]

{ #category : #'class tags' }
RPackage >> classTagCategoryNames [
	^ (Set with: self packageName), (self classTags collect: [:each | each categoryName])
]

{ #category : #'class tags' }
RPackage >> classTagForClass: aClass [
	^ self
		classTagNamed: (self toTagName: aClass category)
		ifAbsent: [ nil ]
]

{ #category : #'class tags' }
RPackage >> classTagNamed: aSymbol [

	^ classTags detect: [ :each | each name = aSymbol ]
]

{ #category : #'class tags' }
RPackage >> classTagNamed: aSymbol ifAbsent: aBlock [
	^ classTags
		detect: [ :each | each name = aSymbol ]
		ifNone: aBlock
]

{ #category : #'class tags' }
RPackage >> classTagNamed: aSymbol ifPresent: aBlock [
	^ classTags
		detect: [ :each | each name = aSymbol ]
		ifFound: [ :tag | aBlock cull: tag ]
		ifNone: [ nil ]
]

{ #category : #'class tags' }
RPackage >> classTags [
	"Returns the tags of the receiver"

	^ classTags
]

{ #category : #accessing }
RPackage >> classes [
	"Return all the classes"
	^ self definedClasses, self extendedClasses
]

{ #category : #'class tags' }
RPackage >> classesForClassTag: aSymbol [
	"Returns the classes tagged using aSymbol"

	^ (self
		classTagNamed: (self toTagName: aSymbol)
		ifAbsent: [ ^ #() ])
		classes
]

{ #category : #queries }
RPackage >> classesTaggedWith: aSymbol [
	"Any class could be tagged for user purpose.
	This method select all classes which are tagged by given symbol"

	^(self classTagNamed: aSymbol) classes
]

{ #category : #accessing }
RPackage >> definedClassNames [
	"Return the class names having methods defined in the receiver."

	^ classes
]

{ #category : #accessing }
RPackage >> definedClasses [

	| definedClasses |
	definedClasses := Set new: classes size.
	classes do: [ :each | Smalltalk globals at: each ifPresent: [ :aClass | definedClasses add: aClass ] ].
	^ definedClasses
]

{ #category : #iteration }
RPackage >> definedClassesDo: aBlock [

	classDefinedSelectors keysDo: aBlock.
	metaclassDefinedSelectors keysDo: aBlock
]

{ #category : #private }
RPackage >> definedMethodsBecomeExtendedForClass: aClassName [
	"the package may contain defined methods and their class is removed to the receiver. The status of such methods should now be extended"
	"Precondition: aClassName is in the defined list"

	| ext |
	(classDefinedSelectors keys includes: aClassName) ifFalse: [^ self].
	ext := classDefinedSelectors at: aClassName.
	classDefinedSelectors removeKey: aClassName ifAbsent: [self reportBogusBehaviorOf:   #definedMethodsBecomeExtendedForClass: ].
	(classExtensionSelectors keys includes: aClassName) ifTrue: [self error: aClassName , ' should not be defined in extension'.]. "we should remove this check later"
	classExtensionSelectors at: aClassName put: ext
]

{ #category : #private }
RPackage >> definedMethodsBecomeExtendedForMetaclass: aClassName [
	"the package may contain defined methods and their class is removed to the receiver. The status of such methods should now be extended"
	"Precondition: aClassName is in the defined list"

	| ext |
	(metaclassDefinedSelectors keys includes: aClassName) ifFalse: [^ self].
	ext := metaclassDefinedSelectors at: aClassName.
	metaclassDefinedSelectors removeKey: aClassName ifAbsent: [self reportBogusBehaviorOf: #definedMethodsBecomeExtendedForMetaclass:].
	(metaclassExtensionSelectors keys includes: aClassName) ifTrue: [self error: aClassName , ' should not be defined in extension'.]. "we should remove this check later"
	metaclassDefinedSelectors at: aClassName put: ext
]

{ #category : #accessing }
RPackage >> definedMethodsForClass: aClass [

	^ (self definedSelectorsForClass: aClass) asOrderedCollection collect: [ :each | aClass >> each ]
]

{ #category : #private }
RPackage >> definedMethodsShouldBecomeExtensionWhenRemovingClass: aClassName [
	"the package may contain defined methods and their class is removed from the receiver. The status of such methods should now be extensions"

	self definedMethodsBecomeExtendedForClass: aClassName.
	self definedMethodsBecomeExtendedForMetaclass: aClassName
]

{ #category : #accessing }
RPackage >> definedOrExtendedClasses [
	^ self definedClasses | self extendedClasses
]

{ #category : #accessing }
RPackage >> definedSelectorsForClass: aClass [

	^ aClass isMeta
		ifTrue: [ metaclassDefinedSelectors at: aClass instanceSide originalName ifAbsent: [ #() ] ]
		ifFalse: [ classDefinedSelectors at: aClass originalName ifAbsent: [ #() ] ]
]

{ #category : #testing }
RPackage >> definesClass: aClass [
	"Returns true if the class, aClass, is one of the locally defined classes"
	"should be probably removed since this is the same as includesClass: or the inverse"
	^ self includesClass: aClass
]

{ #category : #testing }
RPackage >> definesOrExtendsClass: aClass [
	"Returns true whether the class, aClass, is one of the locally defined classes of the receiver or
	if the receiver extends such class (that is defined in another package)"

	| canonizedName |
	canonizedName := aClass instanceSide name.
	^ (classes includes: canonizedName)
		or: [self extendedClassNames includes: canonizedName]
]

{ #category : #converting }
RPackage >> demoteToTagInPackageNamed: aString [
	| newRPackage |

	aString = self name ifTrue: [ ^ self ].

	self unregister.
	newRPackage := self class organizer
		packageNamed: aString
		ifAbsent:  [ (self class named: aString) register ].
	newRPackage importPackage: self.

	newRPackage classes do: [ :each |
		SystemAnnouncer uniqueInstance
			classRepackaged: each
			from: self
			to: newRPackage ]
]

{ #category : #properties }
RPackage >> ensureProperties [
	^ Properties at: self ifAbsentPut: WeakKeyDictionary new
]

{ #category : #accessing }
RPackage >> extendedClassNames [
	"Return the name of the classes which are extended by the receiver package. if a metaclass is extended, just get its sole instance class name."

	^ classExtensionSelectors keys union: metaclassExtensionSelectors keys
]

{ #category : #accessing }
RPackage >> extendedClasses [
	"Return classes and metaclasses that are extended in the receiver. They represent the classes of method extensions"
	^ (metaclassExtensionSelectors keys
		select: [ :each| Smalltalk globals includesKey: each ]
		thenCollect: [:each | (Smalltalk globals at: each) classSide])
			union:  (classExtensionSelectors keys
						select: [ :each| Smalltalk globals includesKey: each ]
						thenCollect: [:each | Smalltalk globals at: each])
]

{ #category : #private }
RPackage >> extendedMethodsBecomeDefinedForClass: aClassName [
	"the package may contain extension methods and their class is added to the receiver. The status of such methods should now be defined"
	"Precondition: aClassName is in the defined list"

	| ext |
	(classExtensionSelectors keys includes: aClassName) ifFalse: [^ self].
	ext := classExtensionSelectors at: aClassName.
	classExtensionSelectors removeKey: aClassName ifAbsent: [self reportBogusBehaviorOf:   #extendedMethodsBecomeDefinedForClass: ].
	(classDefinedSelectors keys includes: aClassName) ifTrue: [self error: aClassName , ' should not be defined in extension'.]. "we should remove this check later"
	classDefinedSelectors at: aClassName put: ext
]

{ #category : #private }
RPackage >> extendedMethodsBecomeDefinedForMetaclass: aClassName [
	"the package may contain extension methods and their class is added to the receiver. The status of such methods should now be defined"
	"Precondition: aClassName is in the defined list"

	| ext |
	(metaclassExtensionSelectors keys includes: aClassName) ifFalse: [^ self].
	ext := metaclassExtensionSelectors at: aClassName.
	metaclassExtensionSelectors removeKey: aClassName  ifAbsent: [self reportBogusBehaviorOf:  #extendedMethodsBecomeDefinedForMetaclass: ].
	(metaclassDefinedSelectors keys includes: aClassName)
			ifTrue: [self error: aClassName , ' should not be defined in extension'.]. "we should remove this check later"
	metaclassDefinedSelectors at: aClassName put: ext
]

{ #category : #private }
RPackage >> extendedMethodsShouldBecomeDefinedWhenAddingClassName: aClassName [
	"the package may contain extension methods and their class is added to the receiver. The status of such methods should now be defined"
	"Precondition: aClassName is in the defined list"

	self extendedMethodsBecomeDefinedForClass: aClassName.
	self extendedMethodsBecomeDefinedForMetaclass: aClassName
]

{ #category : #testing }
RPackage >> extendsClass: aClass [
	"Returns true if the receiver extends aClass (that is defined in another package)"

	| canonizedName |
	canonizedName := aClass instanceSide name.
	^ self extendedClassNames includes: canonizedName
]

{ #category : #'system compatibility' }
RPackage >> extensionCategoriesForClass: aClass [

	self flag: #stef. "only used by goferCleanUp and PackageEnvironment of RB"
	^ aClass organization protocolNames select: [ :cat | self isYourClassExtension: cat ]
]

{ #category : #accessing }
RPackage >> extensionMethods [
	"Extension methods are methods defined on classes that are not defined in the receiver"
	| allExtensionMethods |

	allExtensionMethods := OrderedCollection new.
	classExtensionSelectors keysAndValuesDo: [ :classSymbol :methods |
		methods do: [ :selector | allExtensionMethods add: ((Smalltalk globals at: classSymbol) >> selector) ] ].
	metaclassExtensionSelectors keysAndValuesDo: [ :classSymbol :methods |
		methods do: [ :selector | allExtensionMethods add: ((Smalltalk globals at: classSymbol) classSide >> selector) ] ].

	^ allExtensionMethods
]

{ #category : #accessing }
RPackage >> extensionMethodsForClass: aClass [
	"Change the set of extensions selectors to an Array to avoid compiled methods collisions in the resulting set."

	^ (self extensionSelectorsForClass: aClass) asArray collect: [ :each | aClass >> each ]
]

{ #category : #accessing }
RPackage >> extensionSelectors [
	"Extension methods are methods defined on classes that are not defined in the receiver"

	| classSels |
	classSels := Set new.
	classExtensionSelectors valuesDo: [ :each | classSels addAll: each ].
	metaclassExtensionSelectors valuesDo: [ :each | classSels addAll: each ].
	^ classSels
]

{ #category : #accessing }
RPackage >> extensionSelectorsForClass: aClass [

	^ aClass isMeta
		ifTrue: [ metaclassExtensionSelectors at: aClass instanceSide originalName ifAbsent: [ #() ] ]
		ifFalse: [ classExtensionSelectors at: aClass originalName ifAbsent: [ #() ] ]
]

{ #category : #'class tags' }
RPackage >> extensionsForTag: aRPackageTag [

	^ self extensionMethods select: [ :extensionMethod | | category |
		category := extensionMethod protocol allButFirst.
		self category: category matches: aRPackageTag categoryName ]
]

{ #category : #properties }
RPackage >> hasProperty: aKey [
	self propertyAt: aKey ifAbsent: [ ^ false ].
	^ true
]

{ #category : #accessing }
RPackage >> hierarchyRoots [
	"Returns all the hiearchy roots (by opposition to single classes inheriting from class outside the package) of a package"

	^ self definedClasses
		select: [ :each | (each superclass isNil or: [ each superclass package ~~ self ]) and: [ each hasSubclasses ] ]
]

{ #category : #private }
RPackage >> importClass: aClass [
	"import a class already created but not attached to a package to the receiver.
	Handle also *- convention. Methods defined in *category are not added to the package.
	Pay attention that it will not import anything from the metaClass side"

	self basicImportClass: aClass.
	self basicImportClass: aClass classSide.
	self
		addClassDefinition:  aClass
		toClassTag: (self toTagName: aClass category)
]

{ #category : #private }
RPackage >> importClass: aClass inTag: aTag [
	"import a class already created but not attached to a package to the receiver.
	Handle also *- convention. Methods defined in *category are not added to the package.
	Pay attention that it will not import anything from the metaClass side"

	self basicImportClass: aClass.
	self basicImportClass: aClass classSide.
	self
		addClassDefinition:  aClass
		toClassTag: aTag name
]

{ #category : #private }
RPackage >> importPackage: anRPackage [
	anRPackage definedClasses
		do: [ :className | self importClass: className ].
	"new rpackage inherits the extentions"
	classExtensionSelectors :=  classExtensionSelectors, (anRPackage classExtensionSelectors).
	metaclassExtensionSelectors := metaclassExtensionSelectors, (anRPackage metaclassExtensionSelectors)
]

{ #category : #private }
RPackage >> importProtocol: aProtocol forClass: aClass [
	"import all the methods of a protocol as defined in the receiver."

	(aClass methodsInProtocol: aProtocol) do: [ :method | method isFromTrait ifFalse: [ self addMethod: method ] ]
]

{ #category : #testing }
RPackage >> includesClass: aClass [
	"Returns true if the receiver includes aClass in the classes that are defined within it: only class definition are considered - not class extensions"

	^ self includesClassNamed: aClass instanceSide name
]

{ #category : #testing }
RPackage >> includesClassNamed: aSymbol [
	"Returns true if the receiver includes class named aSymbol in the classes that are defined within it: only class definition are considered - not class extensions"

	^ classes includes: aSymbol
]

{ #category : #testing }
RPackage >> includesClassTagNamed: aString [
	^ self classTags anySatisfy: [ :each | each name = aString ]
]

{ #category : #testing }
RPackage >> includesClassesAffectedBy: aSystemAnnouncement [

	^(aSystemAnnouncement affectsClassesDefinedInPackage: self)
		or: [ aSystemAnnouncement affectsClassesExtendedInPackage: self ]
]

{ #category : #testing }
RPackage >> includesDefinedSelector: aSelector ofClass: aClass [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined in this package"

	| sels |
	sels := self definedSelectorsForClass: aClass.
	^ sels includes: aSelector
]

{ #category : #testing }
RPackage >> includesDefinedSelector: aSelector ofClassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined (not extended by other packages or package extensions)"

	| sels |
	sels := classDefinedSelectors at: aClassName ifAbsent: [^ false].
	^ sels includes: aSelector
]

{ #category : #testing }
RPackage >> includesDefinedSelector: aSelector ofMetaclassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined (not extended by other packages or package extensions)"

	| sels |
	sels := metaclassDefinedSelectors at: aClassName ifAbsent: [^ false].
	^ sels includes: aSelector
]

{ #category : #testing }
RPackage >> includesExtensionSelector: aSelector ofClass: aClass [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods extending other packages"

	| sels |
	sels := self extensionSelectorsForClass: aClass.
	^ sels includes: aSelector
]

{ #category : #testing }
RPackage >> includesExtensionSelector: aSelector ofClassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods extending other packages"

	| sels |
	sels := classExtensionSelectors at: aClassName ifAbsent: [^ false].
	^ sels includes: aSelector
]

{ #category : #testing }
RPackage >> includesExtensionSelector: aSelector ofMetaclassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods extending other packages"

	| sels |
	sels := metaclassExtensionSelectors at: aClassName ifAbsent: [^ false].
	^ sels includes: aSelector
]

{ #category : #'system compatibility' }
RPackage >> includesMethodCategory: categoryName ofClass: aClass [
	^ (self isYourClassExtension: categoryName)
		or: [(self includesClass: aClass)
				and: [(self isForeignClassExtension: categoryName) not]]
]

{ #category : #testing }
RPackage >> includesMethodsAffectedBy: aSystemAnnouncement [

	^aSystemAnnouncement affectsMethodsDefinedInPackage: self
]

{ #category : #testing }
RPackage >> includesSelector: aSelector ofClass: aClass [
	"Return true if the receiver includes the method of selector aSelector. Checks methods defined locally as well as extending other packages"

	^ aClass isMeta
		ifTrue: [ self includesSelector: aSelector ofMetaclassName: aClass instanceSide name  ]
		ifFalse: [ self includesSelector: aSelector ofClassName: aClass name  ]
]

{ #category : #testing }
RPackage >> includesSelector: aSelector ofClassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Checks methods defined locally as well as extending other packages"

	^ (self includesDefinedSelector: aSelector ofClassName: aClassName)
		or: [(self includesExtensionSelector: aSelector ofClassName: aClassName)]
]

{ #category : #testing }
RPackage >> includesSelector: aSelector ofMetaclassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Checks methods defined locally as well as extending other packages"

	^ (self includesDefinedSelector: aSelector ofMetaclassName: aClassName)
		or: [(self includesExtensionSelector: aSelector ofMetaclassName: aClassName)]
]

{ #category : #'system compatibility' }
RPackage >> includesSystemCategory: categoryName [
	^ self category: categoryName matches: self systemCategoryPrefix
]

{ #category : #initialization }
RPackage >> initialize [
	super initialize.
	classes := Set new.
	metaclassExtensionSelectors := Dictionary new.
	classExtensionSelectors := Dictionary new.
	classDefinedSelectors := Dictionary new.
	metaclassDefinedSelectors := Dictionary new.
	classTags := Set new
]

{ #category : #testing }
RPackage >> isDefault [
	"Return whether the receiver is the default package, i.e. the one used when nothing else is specified."
	^ self name = self class defaultPackageName
]

{ #category : #testing }
RPackage >> isDeprecated [
	^ self packageManifestOrNil
		ifNil: [ ^ false ]
		ifNotNil: [ :manifest | manifest isDeprecated ]
]

{ #category : #testing }
RPackage >> isEmpty [
	self name = self class defaultPackageName ifTrue: [ ^false ].
	^self classes isEmpty and: [ self extensionSelectors isEmpty]
]

{ #category : #'system compatibility' }
RPackage >> isForeignClassExtension: categoryName [
	^ categoryName first = $* and: [(self isYourClassExtension: categoryName) not]
]

{ #category : #testing }
RPackage >> isTestPackage [
	"1. Test package ends with suffix -Tests. Suffix is case sensitive.
	 2. Or test package contains '-Tests-' in middle.
	Some examples: "
	"(RPackage named: 'MockPackage-Tests') isTestPackage >>> true"
	"(RPackage named: 'MockPackage-tests') isTestPackage >>> false"
	"(RPackage named: 'MockPackage') isTestPackage >>> false"
	"(RPackage named: 'MockPackage-Tests-Package') isTestPackage >>> true"

	^ (self name endsWith: '-Tests') or: [self name includesSubstring: '-Tests-']
]

{ #category : #'system compatibility' }
RPackage >> isYourClassExtension: categoryName [
	^ categoryName notNil and: [self category: categoryName asLowercase matches: self methodCategoryPrefix]
]

{ #category : #queries }
RPackage >> linesOfCode [
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."
	^self methods inject: 0 into: [:sum :each | sum + each linesOfCode]
]

{ #category : #slices }
RPackage >> metaclassDefinedSlicesDo: aBlock [
	"This method iterates over the defined class and their associated selectors. a slice is a class * list of selectors. aBlock will be applied to all the extensions slices of the receiver. aBlok first argument is the class and the second argument a list of method selectors"

	metaclassDefinedSelectors keysAndValuesDo: aBlock
]

{ #category : #private }
RPackage >> metaclassExtensionSelectors [
	^ metaclassExtensionSelectors
]

{ #category : #slices }
RPackage >> metaclassExtensionSlicesDo: aBlock [
	"This method iterates over the metaclass extensions and their associated selectors. A slice is a class * list of selectors. aBlock will be applied to all the extensions slices of the receiver. aBlok first argument is the class and second argument a list of selectors"

	metaclassExtensionSelectors keysAndValuesDo: aBlock
]

{ #category : #'system compatibility' }
RPackage >> methodCategoryPrefix [
	^ '*', self name asLowercase
]

{ #category : #accessing }
RPackage >> methods [
	"Return all the methods defined in this package. Including extension methods (i.e., methods defined on a class that is not defined by me)"
	| methods |

	methods := OrderedCollection new.

	metaclassExtensionSelectors keysAndValuesDo: [:key :val |
		val do: [:sel | 	methods add: ((Smalltalk globals at: key) classSide >> sel)]].
	classExtensionSelectors keysAndValuesDo: [:key :val |
		val do: [:sel | methods add: ((Smalltalk globals at: key) >> sel)]].

	metaclassDefinedSelectors keysAndValuesDo: [:key :val |
		val do: [:sel | methods add: ((Smalltalk globals at: key) classSide >> sel)]].
	classDefinedSelectors keysAndValuesDo: [:key :val |
		val do: [:sel | methods add: ((Smalltalk globals at: key) >> sel)]].

	^ methods
]

{ #category : #accessing }
RPackage >> methodsForClass: aClass [
	"Returns all compiled methods for a class in the receiver without distinction of extensions or not"

	^ (self includesClass: aClass)
		ifFalse: [self extensionMethodsForClass: aClass]
		ifTrue: [self definedMethodsForClass: aClass]
]

{ #category : #private }
RPackage >> moveClass: aClass fromPackage: oldPackage toTag: aTag [

	oldPackage removeClass: aClass.
	self importClass: aClass inTag: aTag.

	SystemAnnouncer uniqueInstance classRepackaged: aClass from: oldPackage to: self
]

{ #category : #private }
RPackage >> moveClass: aClass toTag: aTag [
	"TODO: deprecate system categories / replace this with a direct call to moveClass:fromPackage:toTag:"

	"This change the class category as appropriate.
		(and by cascade, ensure systemClassRecategorizedAction: is called)."

	aClass category: aTag categoryName
]

{ #category : #accessing }
RPackage >> name [

	^ name
]

{ #category : #accessing }
RPackage >> name: aSymbol [
	"Set the name of a package. This method is private and should not be used.
	If you wish to rename a package, use #renameTo: instead"
	name := aSymbol asSymbol
]

{ #category : #accessing }
RPackage >> orderedClasses [

	| tmp |
	tmp := self definedClasses asArray sort: [:a :b | a name <= b name].
	tmp := tmp, (self extendedClasses asArray sort: [:a :b | a name <= b name]).
	^tmp collect: [:e | e instanceSide ]
]

{ #category : #private }
RPackage >> organizer [
	^ self class organizer
]

{ #category : #accessing }
RPackage >> packageManifestOrNil [
	^ self definedClasses
		detect: [ :each | each isManifest ]
		ifNone: [ nil ]
]

{ #category : #accessing }
RPackage >> packageName [

	^ name
]

{ #category : #'system compatibility' }
RPackage >> packages [
	"Compatibility with monticello and old PackageInfo"
	^ self classTags
]

{ #category : #printing }
RPackage >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: self name.
	aStream nextPut: $)
]

{ #category : #properties }
RPackage >> properties [
	^ Properties at: self ifAbsent: nil
]

{ #category : #properties }
RPackage >> propertyAt: propName [
	^ self
		propertyAt: propName
		ifAbsent: [ nil ]
]

{ #category : #properties }
RPackage >> propertyAt: propName ifAbsent: aBlock [
	self properties ifNil: [^aBlock value].
	^ self properties
		at: propName
		ifAbsent: aBlock
]

{ #category : #properties }
RPackage >> propertyAt: aKey ifAbsentPut: aBlock [
	"Answer the property associated with aKey or, if aKey isn't found store the result of evaluating aBlock as new value."

	^ self propertyAt: aKey ifAbsent: [ self propertyAt: aKey put: aBlock value ]
]

{ #category : #properties }
RPackage >> propertyAt: propName put: propValue [
	^ self ensureProperties
		at: propName
		put: propValue
]

{ #category : #register }
RPackage >> register [
	self organizer registerPackage: self
]

{ #category : #'private - register' }
RPackage >> registerClass: aClass [
	"Private method that declares the mapping between a class and its package."
	self organizer
		registerPackage: self forClass: aClass
]

{ #category : #'private - register' }
RPackage >> registerClassName: aClassNameSymbol [
	"Private method that declares the mapping between a class and its package."
	self organizer
		registerPackage: self forClassName: aClassNameSymbol
]

{ #category : #'private - register' }
RPackage >> removeAllMethodsFromClassNamed: aClassNameSymbol [
	"Remove all the methods (defined and extensions) that are related to class aClassNameSymbol. aClassNameSymbol is the name of a class and not a metaclass - see class comment."

	classDefinedSelectors removeKey: aClassNameSymbol ifAbsent: [nil].
	classExtensionSelectors removeKey: aClassNameSymbol ifAbsent: [nil].
	metaclassDefinedSelectors removeKey: aClassNameSymbol ifAbsent: [nil].
	metaclassExtensionSelectors removeKey: aClassNameSymbol ifAbsent: [nil]
]

{ #category : #removing }
RPackage >> removeClass: aClass [
	"Remove the class and all its methods from the receiver. If we have a protocol which looks like an extension of us, rename it to 'as yet unclassified' to avoid breaking things afterwards."

	aClass organization protocolNames do: [ :each |
		(self isYourClassExtension: each) ifTrue: [ aClass organization renameProtocol: each as: Protocol unclassified ] ].
	(self definedMethodsForClass: aClass instanceSide) do: [ :aCompiledMethod | self removeMethod: aCompiledMethod ].
	"we also have also have to remove methods from class side"
	(self definedMethodsForClass: aClass classSide) do: [ :aCompiledMethod | self removeMethod: aCompiledMethod ].
	self removeClassDefinitionWithoutCheckingMethods: aClass.
	self removeClassTagsForClassNamed: aClass name
]

{ #category : #'add class' }
RPackage >> removeClassDefinition: aClass [
	"Remove the class definition from the package but not its methods."
	^ self removeClassDefinitionName: aClass instanceSide originalName
]

{ #category : #'class tags' }
RPackage >> removeClassDefinition: aClass fromClassTag: aSymbol [
	"Detags the class aClass with the tag aSymbol"

	self
		removeClassDefinitionName: aClass name
		fromClassTag: aSymbol
]

{ #category : #'add class' }
RPackage >> removeClassDefinitionName: aClassName [
	"remove the class definition from the package but not its method"

	| removed aClassNameSymbol |
	('* class' match: aClassName) ifTrue: [^ self error: 'no metaclass name'].
	aClassNameSymbol := aClassName asSymbol.
	"clean up class tags"
	classTags do: [ :tag| tag removeClassNamed: aClassName ].
	(classTags select: #isEmpty)
		do: [ :eachTag | self basicRemoveTag: eachTag ].
	"remove the class definition"
	removed := classes remove: aClassNameSymbol ifAbsent: [nil].
	removed ifNotNil: [
		self definedMethodsShouldBecomeExtensionWhenRemovingClass: aClassName.
		self unregisterClassName: aClassNameSymbol ]
]

{ #category : #'class tags' }
RPackage >> removeClassDefinitionName: aClassName fromClassTag: aSymbol [
	"Detags the class aClass with the tag aSymbol"
	| tag |

	tag := self classTagNamed: (self toTagName: aSymbol) ifAbsent: [ ^ self ].
	tag removeClassNamed: aClassName.
	tag isEmpty
		ifTrue: [ self basicRemoveTag: tag ]
]

{ #category : #'private - register' }
RPackage >> removeClassDefinitionNamed: aClassNameSymbol [
	"aClassNameSymbol should be the name of a class and not a metaclass to be coherent with the class invariant that classes are only class names and not metaclass ones.

	This method removes the class definition from the receiver and its organizer"

	| removed |
	removed := classes remove: aClassNameSymbol ifAbsent: [nil].
	removed ifNotNil: [ self unregisterClassName: aClassNameSymbol.]
]

{ #category : #private }
RPackage >> removeClassDefinitionWithoutCheckingMethods: aClass [
	"Same than 'removeClassDefinition', excepts that it does not make any check to set defined methods as extensions"

	| removed aClassName|
	aClassName := aClass instanceSide name.
	removed := classes remove: aClassName ifAbsent: [nil].
	removed ifNotNil: [
		self unregisterClass: aClass.]
]

{ #category : #removing }
RPackage >> removeClassNamed: className [
	self removeClassDefinitionName: className.
	self removeAllMethodsFromClassNamed: className.
	self removeClassTagsForClassNamed: className
]

{ #category : #'class tags' }
RPackage >> removeClassTag: aSymbol [
	"Remove the class tag from the receiver."

	self basicRemoveTag: (self
		classTagNamed:  (self toTagName: aSymbol)
		ifAbsent: [ ^ self ])
]

{ #category : #'class tags' }
RPackage >> removeClassTagsForClassNamed: aString [
	(self classTags
		select: [ :eachTag | eachTag hasClassNamed: aString ])
		do: [ :eachTag |
			self
				removeClassDefinitionName: aString
				fromClassTag: eachTag name ]
]

{ #category : #'class tags' }
RPackage >> removeClassesMatchingTag: aTag [

	aTag classes do: [ :class | self removeClassDefinition: class ]
]

{ #category : #removing }
RPackage >> removeFromSystem [

	| extendedClasses categories |
	categories := (self classTags collect: [ :each | each categoryName ] as: Set)
		              add: self name;
		              yourself.
	extendedClasses := self extendedClasses.

	self definedClasses do: #removeFromSystem.
	self extensionMethods do: #removeFromSystem.

	extendedClasses do: [ :each | each organization removeProtocolIfEmpty: ('*' , name) asSymbol ].
	"This is probably not the best place to unregister from the SystemOrganizer but later it's harder to find the categories. 
	But anyway, SystemOrganizer should be removed in the next few months and we will be able to remove this code."
	categories do: [ :cat | self organizer systemOrganizer removeCategory: cat ].
	self unregister
]

{ #category : #'add method - compiled method' }
RPackage >> removeMethod: aCompiledMethod [
	"Remove the method to the receiver as a defined method."
	|  methodClass |

	methodClass := aCompiledMethod methodClass.
	methodClass isMeta
		ifTrue: [
			self
				removeSelector: aCompiledMethod selector
				ofMetaclassName: methodClass instanceSide originalName ]
		ifFalse: [
			self
				removeSelector: aCompiledMethod selector
				ofClassName: methodClass originalName ].

	^ aCompiledMethod
]

{ #category : #'add method - compiled method' }
RPackage >> removeMethods: aCollection [
	aCollection do: [ :each | self removeMethod: each ]
]

{ #category : #properties }
RPackage >> removePropertiesIfEmpty [
	^ Properties at: self ifPresent: [ :dict |
		dict ifEmpty: [ Properties removeKey: self ] ]
]

{ #category : #properties }
RPackage >> removeProperty: propName [
	^ self
		removeProperty: propName
		ifAbsent: [ nil ]
]

{ #category : #properties }
RPackage >> removeProperty: propName ifAbsent: aBlock [
	| property |
	self properties ifNil: [^aBlock value].
	property := self properties
		removeKey: propName
		ifAbsent: aBlock.
	self removePropertiesIfEmpty.
	^ property
]

{ #category : #'add method - selector' }
RPackage >> removeSelector: aSelector ofClassName: aClassName [
	"Remove the method in the package but does not propagate to the class itself.  Note that this method does not remove the method from the class, it just records in the package that the method is not in the package anymore."

	self basicRemoveSelector: aSelector ofClassName: aClassName.
	(classExtensionSelectors at: aClassName ifAbsent: [ #() ]) ifEmpty: [
		classExtensionSelectors removeKey: aClassName ifAbsent: [].
	].
	(classDefinedSelectors at: aClassName ifAbsent: [ #() ]) ifEmpty: [
		classDefinedSelectors removeKey: aClassName ifAbsent: [].
	].

	((metaclassExtensionSelectors at: aClassName ifAbsent: [#()]) isEmpty and: [(classExtensionSelectors at: aClassName ifAbsent: [#()]) isEmpty])
		ifTrue: [
				self organizer unregisterExtendingPackage: self forClassName: aClassName
	]
]

{ #category : #'add method - selector' }
RPackage >> removeSelector: aSelector ofMetaclassName: aClassName [
	"Remove the method in the package. Note that this method does not remove the method from the class, it just records in the package that the method is not in the package anymore. aClassName is the sole instance class name and not its metaclass one: i.e. adding Point class>>new is done as removeSelector: #new ofMetaclassName: #Point"

	self basicRemoveSelector: aSelector ofMetaclassName: aClassName.
	(metaclassExtensionSelectors at: aClassName ifAbsent: [ #() ]) ifEmpty: [
		metaclassExtensionSelectors removeKey: aClassName ifAbsent: [].
	].
	(metaclassDefinedSelectors at: aClassName ifAbsent: [ #() ]) ifEmpty: [
		metaclassDefinedSelectors removeKey: aClassName ifAbsent: [].
	].

	((metaclassExtensionSelectors at: aClassName ifAbsent: [#()]) isEmpty and: [(classExtensionSelectors at: aClassName ifAbsent: [#()]) isEmpty])
		ifTrue: [
				self organizer unregisterExtendingPackage: self forClassName: aClassName
	]
]

{ #category : #private }
RPackage >> renameExtensionsPrefixedWith: oldName to: newName [

	| protocols |
	protocols := self extensionMethods collect: [ :each | each origin organization protocolNamed: each protocol ] as: Set.

	protocols do: [ :each | each rename: '*' , newName , (each name allButFirst: oldName size + 1) ]
]

{ #category : #private }
RPackage >> renameTagsPrefixedWith: oldName to: newName [
	| oldPrefix newPrefix |

	oldName ifNil: [ ^ self ].

	self
		classTagNamed: oldName
		ifPresent: [ :tag | tag renameTo: newName category: newName ].

	oldPrefix := oldName, '-'.
	newPrefix := newName, '-'.
	(self classTags
		select: [ :each | each categoryName beginsWith: oldPrefix ])
		do: [ :each |
			each
				renameTo: each name
				category: newPrefix, (each name) ]
]

{ #category : #register }
RPackage >> renameTo: aSymbol [
	"Rename a package with a different name, provided as a symbol"

	| oldName newName oldCategoryNames repositoryGroup |
	oldName := self name.
	newName := aSymbol asSymbol.
	repositoryGroup := self mcPackage workingCopy repositoryGroup.
	self organizer validatePackageDoesNotExist: aSymbol.
	oldCategoryNames := (self classTags collect: [:each | each categoryName] as: Set)
		add: self name;
		difference: {newName}.
	self name: aSymbol.
	SystemAnnouncer uniqueInstance
		suspendAllWhile: [ self definedClasses
				do:
					[ :each | each category: newName , (each category allButFirst: oldName size) ].
			oldCategoryNames
				do: [ :each | self systemOrganization removeCategory: each ] ].
	self renameTagsPrefixedWith: oldName to: newName.
	self renameExtensionsPrefixedWith: oldName to: newName.
	self organizer
		basicUnregisterPackageNamed: oldName;
		basicRegisterPackage: self.
	self mcPackage workingCopy repositoryGroup: repositoryGroup.
	SystemAnnouncer uniqueInstance
		announce:
			(RPackageRenamed to: self oldName: oldName newName: newName)
]

{ #category : #private }
RPackage >> reportBogusBehaviorOf: aSelector [

	self traceCr: 'RPackage log: Something wrong around ', aSelector asString , 'since the removeKey: is called on not present information.'
]

{ #category : #accessing }
RPackage >> roots [
	"Returns all the root classes of a package. A root class is a class whose superclass is not defined in the package.
	Root classes are potentially root of inheritance trees defined in a package."

	^ self definedClasses
		select: [ :each | each superclass isNil or: [ each superclass package ~~ self ] ]
]

{ #category : #accessing }
RPackage >> selectors [

	| sel |
	sel := Set new.
	metaclassExtensionSelectors valuesDo: [:each | sel addAll: each].
	classExtensionSelectors valuesDo: [:each | sel addAll: each].
	metaclassDefinedSelectors valuesDo: [:each | sel addAll: each].
	classDefinedSelectors valuesDo: [:each | sel addAll: each].
	^ sel
]

{ #category : #accessing }
RPackage >> selectorsForClass: aClass [
	"Returns all selectors for a class in the receiver without distinction of extensions or not"

	^ (self includesClass: aClass)
		ifFalse: [self extensionSelectorsForClass: aClass]
		ifTrue: [self definedSelectorsForClass: aClass]
]

{ #category : #'system compatibility' }
RPackage >> systemCategories [

	^ self systemOrganization categories select: [:cat | self includesSystemCategory: cat]
]

{ #category : #'system compatibility' }
RPackage >> systemCategoryPrefix [
	^ self name
]

{ #category : #'system compatibility' }
RPackage >> systemOrganization [

	^ self organizer systemOrganizer
]

{ #category : #queries }
RPackage >> tagsForClasses [
	"Any class could be tagged for user purpose.
	Now we implement it on top of RPackageTag"

	^self classTags reject: [:each | each isRoot] thenCollect: [:each | each name]
]

{ #category : #private }
RPackage >> toTagName: aSymbol [

	^ (aSymbol beginsWith: self name asString , '-')
		  ifTrue: [ (aSymbol allButFirst: self name size + 1) asSymbol ]
		  ifFalse: [ aSymbol ]
]

{ #category : #register }
RPackage >> unregister [
	self organizer unregisterPackage: self
]

{ #category : #'private - register' }
RPackage >> unregisterClass: aClass [
	"Private method that declares the mapping between a class and its package."
	self organizer
		unregisterPackage: self forClass: aClass
]

{ #category : #'private - register' }
RPackage >> unregisterClassName: aClassNameSymbol [
	"Private method that declares the mapping between a class and its package."
	self organizer unregisterPackage: self forClassName: aClassNameSymbol
]

{ #category : #updating }
RPackage >> updateDefinedClassNamed: oldString withNewName: newString [
	"this method should only be used with class names , and not metaclass names"

	(oldString substrings size > 1 or: [ newString substrings size > 1 ])
		ifTrue: [ Error signal: 'You should not use metaclass names' ].	"=> update the 'classDefinedSelectors' dictionary (replace the old key by the new one)
       => update the 'metaclassDefinedSelectors' dictionary (replace the old key by the new one)"
	classes remove: oldString.
	classes add: newString.	"Don't forget that class tags also register the classes names."
	self classTags do: [ :aTag | aTag updateDefinedClassNamed: oldString withNewName: newString ].
	self flag: #cyrille.	"what happens if this is a metaclass: it breaks the invariant of the package"	"worse so far the second block below breaks"	"the name of the method should be updateDefinedClassNamed: withNewName:"	"CD: i only use this method when a class is renamed in the system (systemClassRenamedActionFrom:). Normally we are not able to rename a metaclass in the system (?) so we should never end up in a case with metaclass names ?  "
	(classDefinedSelectors at: oldString ifAbsent: [ nil ])
		ifNotNil: [
			classDefinedSelectors at: newString put: (classDefinedSelectors at: oldString).
			classDefinedSelectors
				removeKey: oldString
				ifAbsent: [ self reportBogusBehaviorOf: #updateDefinedClassNamed:withNewName: ] ].
	(metaclassDefinedSelectors at: oldString ifAbsent: [ nil ])
		ifNotNil: [
			metaclassDefinedSelectors at: newString put: (metaclassDefinedSelectors at: oldString).
			metaclassDefinedSelectors
				removeKey: oldString
				ifAbsent: [ self reportBogusBehaviorOf: #updateDefinedClassNamed:withNewName: ] ]
]

{ #category : #updating }
RPackage >> updateDefinedSelector: oldSelector inClass: aClass withNewSelector: newSelector [

	aClass isMeta
		ifTrue: [
			(metaclassDefinedSelectors at: aClass instanceSide name asSymbol) remove: oldSelector.
			(metaclassDefinedSelectors at: aClass instanceSide name asSymbol) add: newSelector. ]
		ifFalse: [
			(classDefinedSelectors  at: aClass name asSymbol) remove: oldSelector.
			(classDefinedSelectors  at: aClass name asSymbol) add: newSelector. ]
]

{ #category : #updating }
RPackage >> updateExtensionForClassNamed: oldString withNewName: newString [

	"=> update the 'classExtensionsSelectors' dictionary (replace the old key by the new one)
        => update the 'metaclassclassExtensionsSelectors' dictionary (replace the old key by the new one)"
	"classes remove: oldString.
	classes add: newString."

	(metaclassExtensionSelectors at: oldString ifAbsent: [nil]) ifNotNil: [
		metaclassExtensionSelectors at: newString put: (metaclassExtensionSelectors at: oldString).
		metaclassExtensionSelectors removeKey: oldString  ifAbsent: [self reportBogusBehaviorOf: #updateExtensionForClassNamed:withNewName:   ]
		].

	(classExtensionSelectors at: oldString ifAbsent: [nil]) ifNotNil: [
		classExtensionSelectors at: newString put: (classExtensionSelectors at: oldString).
		classExtensionSelectors removeKey: oldString ifAbsent: [self reportBogusBehaviorOf: #updateExtensionForClassNamed:withNewName:   ]
		]
]

{ #category : #updating }
RPackage >> updateExtensionSelector: oldSelector inClass: aClass withNewSelector: newSelector [

	aClass isMeta
		ifTrue: [
			(metaclassExtensionSelectors at: aClass instanceSide name asSymbol) remove: oldSelector.
			(metaclassExtensionSelectors at: aClass instanceSide name asSymbol) add: newSelector.
			]
		ifFalse: [
			(classExtensionSelectors  at: aClass name asSymbol) remove: oldSelector.
			(classExtensionSelectors  at: aClass name asSymbol) add: newSelector.
			]
]

{ #category : #updating }
RPackage >> updateSelector: oldSelector inClass: aClass withNewSelector: newSelector [

	"here we check if the old seletor is defined in the package or if it is an extension. Then we dispatch the work to the corresponding method.
	For one specific class, a selector is either a defined selector or an extension selector, there is no way to have a couple of class->selector present in both dictionary"

	(self includesDefinedSelector: oldSelector ofClass: aClass)
		ifTrue: [^ self updateDefinedSelector: oldSelector inClass: aClass  withNewSelector: newSelector].
	(self includesExtensionSelector: oldSelector ofClass: aClass)
		ifTrue: [^ self updateExtensionSelector: oldSelector inClass: aClass  withNewSelector: newSelector]
]
