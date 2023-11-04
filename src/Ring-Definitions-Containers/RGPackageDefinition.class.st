"
RGPackage is the representation for packages.
A package manages classes, methods and children packages as elements.
A package can also know in which package is defined

"
Class {
	#name : 'RGPackageDefinition',
	#superclass : 'RGDefinition',
	#instVars : [
		'package',
		'elements',
		'definedClasses'
	],
	#category : 'Ring-Definitions-Containers-Base',
	#package : 'Ring-Definitions-Containers',
	#tag : 'Base'
}

{ #category : 'class initialization' }
RGPackageDefinition class >> initialize [
	"Those got merged into RGPackageDefinition"

	self deprecatedAliases: { #RGAbstractContainer. #RGContainer }
]

{ #category : 'adding-removing' }
RGPackageDefinition >> addClass: aRGBehaviorDefinition [
	"aRGBehaviorDefinition has to be a class, trait or metaclass"

	self flag: 'when i am adding a metaclass? check this?'.

	(aRGBehaviorDefinition isClass or: [ aRGBehaviorDefinition isTrait ]) ifFalse: [ ^ self ].
	aRGBehaviorDefinition package: self.
	definedClasses at: aRGBehaviorDefinition fullName put: aRGBehaviorDefinition
]

{ #category : 'adding-removing' }
RGPackageDefinition >> addClassNamed: className [
	"Creates a class with the given name"
	self addClass: (RGClassDefinition named: className)
]

{ #category : 'adding-removing' }
RGPackageDefinition >> addElement: anObject [

	anObject addInContainer: self
]

{ #category : 'adding-removing' }
RGPackageDefinition >> addElement: aRGDefinition in: aCollection [

	aRGDefinition package: self.
	aCollection isDictionary
		ifTrue: [ aCollection at: aRGDefinition fullName put: aRGDefinition ]
		ifFalse: [ aCollection add: aRGDefinition ]
]

{ #category : 'managing container' }
RGPackageDefinition >> addInContainer: anRGContainer [

	anRGContainer addPackage: self
]

{ #category : 'adding-removing' }
RGPackageDefinition >> addMethod: aRGMethodDefinition [
	"aRGMethodDefinition needs to satisfy the status isMethod"

	aRGMethodDefinition isMethod ifFalse: [ ^ self ].
	aRGMethodDefinition package: self. "Needed to evaluate #isExtension if it was not set in advance"
	self addElement: aRGMethodDefinition in: (aRGMethodDefinition isExtension
			 ifTrue: [ self extensionMethods ]
			 ifFalse: [ self definedMethods ])
]

{ #category : 'adding-removing' }
RGPackageDefinition >> addPackage: aRGPackage [
	"adds a child package"

	aRGPackage isPackage ifFalse:[ ^self ].
	self addElement: aRGPackage in: self packages
]

{ #category : 'adding-removing' }
RGPackageDefinition >> addPackageNamed: packageName [
	"adds a child package with the given name"

	(self includesPackageNamed: packageName) ifTrue: [ ^self ].
	self addPackage: (RGPackageDefinition named: packageName)
]

{ #category : 'adding-removing' }
RGPackageDefinition >> addTrait: aRGTraitDefinition [
	"convenient method"

	self addClass: aRGTraitDefinition
]

{ #category : 'adding-removing' }
RGPackageDefinition >> addTraitNamed: traitName [
	"Creates a trait with the given name"

	self addClass: (RGTraitDefinition named: traitName)
]

{ #category : 'convenient accesses' }
RGPackageDefinition >> allClasses [
	"convenient method"

	^self classes
]

{ #category : 'iterating' }
RGPackageDefinition >> allClassesDo: aBlock [
	"Evaluate the argument, aBlock, for each class and its metaclass"

	self allClasses do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : 'convenient accesses' }
RGPackageDefinition >> allMethods [
	"convenient method"

	^self methods
]

{ #category : 'convenient accesses' }
RGPackageDefinition >> allTraits [
	"convenient method"

	^self allClasses select:[ :each | each isTrait ]
]

{ #category : 'iterating' }
RGPackageDefinition >> allTraitsDo: aBlock [
	"Evaluate the argument, aBlock, for each trait"

	self allTraits do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : 'lookup by name' }
RGPackageDefinition >> classNamed: className [
	"Retrieves an RGBehaviorDefinition object.
	className could be classSide name"

	^self classOrTraitNamed: className
]

{ #category : 'accessing' }
RGPackageDefinition >> classNames [

	^ self classes isDictionary
		ifTrue: [ self classes keys ]
		ifFalse:[ self classes collect:[ :class|  class name ] ]
]

{ #category : 'lookup by name' }
RGPackageDefinition >> classOrTraitNamed: className [
	"A class or metaclass can be reached by its name"

	className ifNil: [ ^ nil ].

	^ definedClasses at: (self theNonMetaClassNameOf: className) ifPresent: [ :theClass |
		  (self isMetaclassName: className)
			  ifTrue: [ theClass classSide ]
			  ifFalse: [ theClass ] ]
]

{ #category : 'accessing' }
RGPackageDefinition >> classes [
	"Retrieves a collection (by default a dictionary) of classes defined in the receiver and classes holding extension methods"

	^self definedClasses, self extendedClasses
]

{ #category : 'convenient accesses' }
RGPackageDefinition >> classes: aCollection [
	"convenient method"

	self definedClasses: aCollection
]

{ #category : 'iterating' }
RGPackageDefinition >> classesDo: aBlock [

	self classes do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : 'image package loading' }
RGPackageDefinition >> cleanEmptyPackages [
	(self packages select: [ :pck | pck classes isEmpty and: [ pck methods isEmpty ] ]) do: [ :pck | self removePackage: pck ]
]

{ #category : 'convenient accesses' }
RGPackageDefinition >> definedClassNames [
	"Retrieves class names (including traits)"

	^ self definedClasses isDictionary
		  ifTrue: [ self definedClasses keys ]
		  ifFalse: [ self definedClasses collect: [ :class | class name ] ]
]

{ #category : 'accessing' }
RGPackageDefinition >> definedClasses [
	"Retrieves classes (traits are included)"

	^ definedClasses
]

{ #category : 'accessing' }
RGPackageDefinition >> definedClasses: aCollection [
	"Set the classes collection"

	definedClasses := aCollection
]

{ #category : 'iterating' }
RGPackageDefinition >> definedClassesDo: aBlock [

	self definedClasses do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : 'accessing' }
RGPackageDefinition >> definedMethods [
	"Retrieves methods defined in the container"

	^ self elements at: #methods ifAbsentPut: [ OrderedCollection new ]
]

{ #category : 'accessing' }
RGPackageDefinition >> definedTraits [
	"Retrieves the traits defined in the receiver"

	^self traits
]

{ #category : 'testing' }
RGPackageDefinition >> definesClass: aRGBehaviorDefinition [
	"Returns true if the receiver includes aRGBehaviorDefinition in the defined classes"

	^ self includesClass: aRGBehaviorDefinition
]

{ #category : 'testing' }
RGPackageDefinition >> definesOrExtendsClass: aRGBehaviorDefinition [
	"Returns true whether the class, aRGBehaviorDefinition, is one locally defined classes of the receiver or
	if the receiver extends such class (that is defined in another package)"

	^ (self definesClass: aRGBehaviorDefinition) or:[ self extendsClass: aRGBehaviorDefinition ]
]

{ #category : 'lookup' }
RGPackageDefinition >> elementNamed: elementName in: aCollection [
	| aSymbol |

	aSymbol:= elementName asSymbol.
	^aCollection isDictionary
		ifTrue: [ aCollection at: aSymbol ifAbsent: [ nil ] ]
		ifFalse:[ aCollection detect:[ :each| each fullName = aSymbol ] ifNone:[ nil ] ]
]

{ #category : 'accessing' }
RGPackageDefinition >> elements [
	"Retrieves the elements"

	^elements
]

{ #category : 'iterating elements' }
RGPackageDefinition >> elementsDo: aBlock [
	elements valuesDo: [ :collection | collection do: [ :each | aBlock value: each ] ]
]

{ #category : 'accessing' }
RGPackageDefinition >> extendedClassNames [

	^ self extendedClasses isDictionary
		ifTrue: [ self extendedClasses keys ]
		ifFalse:[ self extendedClasses collect:[ :each| each name ]]
]

{ #category : 'accessing' }
RGPackageDefinition >> extendedClasses [
	"Retrieves the classes holding the extension methods and returns a dictionary as well"
	|  classesOfExt |
	classesOfExt := Dictionary new.

	self extensionMethods
		do:[ :mth| (classesOfExt includesKey: mth parentName)
					ifFalse:[ classesOfExt at: mth parentName put: mth parent ] ].

	^classesOfExt
]

{ #category : 'accessing' }
RGPackageDefinition >> extendedSelectors [
	"Retrieves the names of the methods"

	^ self extensionMethods keys
]

{ #category : 'testing' }
RGPackageDefinition >> extendsClass: aRGBehaviorDefinition [
	"Returns true if the receiver extends aRGBehaviorDefinition (that is defined in another package)"

	^ self extendedClasses includes: aRGBehaviorDefinition
]

{ #category : 'accessing' }
RGPackageDefinition >> extensionMethods [
	"Retrieves extension methods"

	^self elements at: #extensionMethods
]

{ #category : 'accessing' }
RGPackageDefinition >> extensionMethods: aCollection [
	"Set the extension methods collection"

	self elements at: #extensionMethods put: aCollection
]

{ #category : 'image package loading' }
RGPackageDefinition >> findPackageOfClass: aRGBehaviorDefinition using: packageKeys [
	"Look for the package of a class. It is nil when there is not a package created for a category in MC"

	self packages isEmpty ifTrue: [ ^ nil ].
	^ self packages at: aRGBehaviorDefinition category ifAbsent: [
		  packageKeys
			  detect: [ :pair | aRGBehaviorDefinition category isCategoryOf: pair key ]
			  ifFound: [ :pair | self packages at: pair key ]
			  ifNone: [ nil ] ]
]

{ #category : 'image package loading' }
RGPackageDefinition >> findPackageOfMethod: aRGMethodDefinition using: packageKeys [
	"Look for the package of an extension method. nil otherwise"

	| pair lname |
	self packages ifEmpty: [ ^ nil ].
	(aRGMethodDefinition protocol beginsWith: '*') ifFalse: [ ^ nil ].
	lname := aRGMethodDefinition protocol asLowercase.
	pair := packageKeys
		        detect: [ :assoc | lname isCategoryOf: assoc value ]
		        ifNone: [ nil ].
	^ pair ifNotNil: [ self packages at: pair key ]
]

{ #category : 'initialization' }
RGPackageDefinition >> flushElements [

	elements:= IdentityDictionary new
]

{ #category : 'testing' }
RGPackageDefinition >> hasClasses [

	^self definedClasses notEmpty
]

{ #category : 'testing' }
RGPackageDefinition >> hasMethods [

	^self methods notEmpty
]

{ #category : 'testing' }
RGPackageDefinition >> includesClass: aRGBehaviorDefinition [
	"Returns true if the receiver includes aRGBehaviorDefinition in the defined classes"

	^self definedClasses includes: aRGBehaviorDefinition
]

{ #category : 'testing' }
RGPackageDefinition >> includesClassNamed: className [
	"Returns true if the receiver includes the className in the defined classes"

	self flag: 'can a metaclass be stored without its nonMetaClass? Check this'.
	^self includesElementNamed: (self theNonMetaClassNameOf: className) in: self definedClasses
]

{ #category : 'testing' }
RGPackageDefinition >> includesElement: anRGDefinition [

	^anRGDefinition isIncludedInContainer: self
]

{ #category : 'testing' }
RGPackageDefinition >> includesElementNamed: elementName [

	^ elements anySatisfy: [ :collection | self includesElementNamed: elementName asSymbol in: collection ]
]

{ #category : 'testing' }
RGPackageDefinition >> includesElementNamed: elementName in: aCollection [

	^aCollection isDictionary
		ifTrue: [ aCollection includesKey:  elementName asSymbol ]
		ifFalse:[ aCollection anySatisfy: [ :each| each name = elementName asSymbol ] ]
]

{ #category : 'testing' }
RGPackageDefinition >> includesMethod: aRGMethodDefinition [

	^self methods includes: aRGMethodDefinition
]

{ #category : 'testing' }
RGPackageDefinition >> includesMethodNamed: fullSelectorName [

	^self includesElementNamed: fullSelectorName in: self methods
]

{ #category : 'testing' }
RGPackageDefinition >> includesPackage: aRGPackage [
	"Verifies if a child package == aRGPackage exists"

	^self packages includes: aRGPackage
]

{ #category : 'testing' }
RGPackageDefinition >> includesPackageNamed: packageName [
	"Verifies if a child package with name = packageName exists"

	^self includesElementNamed: packageName in: self packages
]

{ #category : 'initialization' }
RGPackageDefinition >> initialize [

	super initialize.
	elements := IdentityDictionary new.
	definedClasses := IdentityDictionary new.
	self methods: IdentityDictionary new.
	self packages: IdentityDictionary new.
	self withoutPackages. "By default the receiver has no child packages"
	self extensionMethods: IdentityDictionary new
]

{ #category : 'utilities' }
RGPackageDefinition >> isMetaclassName: aSymbol [
	"Validates if the arguments corresponds to a nonMetaClass"

	^(aSymbol
		indexOfSubCollection: ' class'
		startingAt: 1) > 0
]

{ #category : 'testing' }
RGPackageDefinition >> isPackage [

	^true
]

{ #category : 'image class and method loading' }
RGPackageDefinition >> loadClass: aRGBehaviorDefinition using: packageKeys [

	self setPackageOfClass: aRGBehaviorDefinition using: packageKeys.
	self addClass: aRGBehaviorDefinition
]

{ #category : 'image class and method loading' }
RGPackageDefinition >> loadMethod: aRGMethodDefinition inClass: aRGBehaviorDefinition using: packageKeys [

	self addMethod: aRGMethodDefinition.
	aRGBehaviorDefinition addMethod: aRGMethodDefinition.
	self setPackageOfMethod: aRGMethodDefinition ofClass: aRGBehaviorDefinition using: packageKeys
]

{ #category : 'image class and method loading' }
RGPackageDefinition >> loadTraitUsers [
	"Set the users of a trait"

	(self classes select: [ :class | class hasTraitComposition ]) do: [ :user |
		user traitNames do: [ :tname |
			(self traitNamed: tname) ifNotNil: [ :rgTrait |
				rgTrait addUser: user.
				rgTrait classSide addUser: user classSide ] ] ]
]

{ #category : 'lookup' }
RGPackageDefinition >> lookupClassNamed: className [
	"Look for the class named className in the receiver's defined classes or in any of its package children"
	| class |

	class:= self classNamed: className.
	class ifNotNil:[ ^class ].

	self packages
		do:[ :each| class:= each lookupClassNamed: className.
				    class ifNotNil:[ ^class ] ].
	^nil
]

{ #category : 'lookup by name' }
RGPackageDefinition >> metaclassNamed: metaclassName [

	(self classOrTraitNamed: metaclassName) ifNotNil: [ :theClass |
		^ theClass isMeta
			  ifTrue: [ theClass ]
			  ifFalse: [ theClass classSide ] ].

	^ nil
]

{ #category : 'lookup by name' }
RGPackageDefinition >> methodNamed: fullSelectorName [

	^self elementNamed: fullSelectorName in: self methods
]

{ #category : 'accessing' }
RGPackageDefinition >> methods [
	"Retrieves all the methods defined in the receiver.
	#methods holds the methods of defined classes"

	^ self definedMethods , self extensionMethods
]

{ #category : 'accessing' }
RGPackageDefinition >> methods: aCollection [
	"Set the methods collection"

	self elements at: #methods put: aCollection
]

{ #category : 'iterating' }
RGPackageDefinition >> methodsDo: aBlock [

	self methods
		do:[ :each| aBlock value: each ]
]

{ #category : 'metrics' }
RGPackageDefinition >> numberOfLinesOfCode [

	^ self
		annotationNamed: #numberOfLinesOfCode
		ifAbsentPut: [
			| number |
			number := self classes inject: 0 into: [ :sum :each | sum + each numberOfLinesOfCode + each classSide numberOfLinesOfCode ].
			number := self methods inject: number into: [ :sum :each | sum + each numberOfLinesOfCode ].
			number ]
]

{ #category : 'accessing' }
RGPackageDefinition >> package [
	"Retrieves the  package in which self is contained"

	^package
]

{ #category : 'accessing' }
RGPackageDefinition >> package: aRGPackage [
	"Sets the outer package of the receiver if exists"

	package:= aRGPackage
]

{ #category : 'lookup by name' }
RGPackageDefinition >> packageNamed: packageName [
	"Look for a child package named packageName"

	^self elementNamed: packageName in: self packages
]

{ #category : 'accessing' }
RGPackageDefinition >> packages [
	"Retrieves the children packages defined in the receiver"

	^ self elements at: #packages ifAbsentPut: [ OrderedCollection new ]
]

{ #category : 'accessing' }
RGPackageDefinition >> packages: aCollection [

	elements at: #packages put: aCollection
]

{ #category : 'accessing' }
RGPackageDefinition >> parent [
	"Retrieves the environment linked to this package"

	^self environment
]

{ #category : 'printing' }
RGPackageDefinition >> printOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		nextPutAll: self name;
		nextPutAll: ')'
]

{ #category : 'accessing' }
RGPackageDefinition >> realPackage [

	^ RPackageOrganizer default packageNamed: self name
]

{ #category : 'adding-removing' }
RGPackageDefinition >> removeClass: aRGAbstractClassDefinition [

	self removeElement: aRGAbstractClassDefinition from: self definedClasses
]

{ #category : 'adding-removing' }
RGPackageDefinition >> removeElement: anObject [

	anObject removeFromContainer: self
]

{ #category : 'adding-removing' }
RGPackageDefinition >> removeElement: anObject from: aCollection [

	aCollection isDictionary
		ifTrue: [ aCollection removeKey: anObject fullName ifAbsent:[ ] ]
		ifFalse:[ aCollection remove: anObject ifAbsent:[ ] ]
]

{ #category : 'managing elements groups' }
RGPackageDefinition >> removeElementsCategorized: aSymbol [
	"Deletes a group of elements"

	elements removeKey: aSymbol ifAbsent:[ ]
]

{ #category : 'adding-removing' }
RGPackageDefinition >> removeMethod: aRGMethodDefinition [

	self removeElement: aRGMethodDefinition from: (aRGMethodDefinition isExtension
			 ifTrue: [ self extensionMethods ]
			 ifFalse: [ self definedMethods ])
]

{ #category : 'adding-removing' }
RGPackageDefinition >> removePackage: aRGPackage [
	"removes a child aRGPackage"

	aRGPackage isPackage ifFalse:[ ^self ].
	self removeElement: aRGPackage from: self packages
]

{ #category : 'adding-removing' }
RGPackageDefinition >> removeTrait: aRGTraitDefinition [
	"convenient method"

	self removeClass: aRGTraitDefinition
]

{ #category : 'image package loading' }
RGPackageDefinition >> setPackageOfClass: rgClass using: packageKeys [
	"Set the package to aRGBehaviorDefinition"

	(self findPackageOfClass: rgClass using: packageKeys)
		ifNotNil:[ :pck| pck addClass: rgClass ]
]

{ #category : 'image package loading' }
RGPackageDefinition >> setPackageOfMethod: rgMethod ofClass: rgClass using: packageKeys [
	"Set the package to aRGMethodDefinition and its value isExtension"

	(self findPackageOfMethod: rgMethod using: packageKeys)
		ifNil:[ rgClass package ifNotNil:[ :pck|
				pck addMethod: rgMethod.
				rgMethod isExtension: false ] ]
		ifNotNil:[ :pck|
			pck addMethod: rgMethod.
			rgMethod isExtension: (rgMethod package ~= rgClass package) ]
]

{ #category : 'utilities' }
RGPackageDefinition >> theNonMetaClassNameOf: aSymbol [
	"Rejects the prefix ' class' or ' classTrait' of the argument"
	| index |

	index := aSymbol
				indexOfSubCollection: ' class'
				startingAt: 1
				ifAbsent: [ ^aSymbol asSymbol ].

	^(aSymbol
		copyFrom: 1
		to: index - 1) asSymbol
]

{ #category : 'lookup by name' }
RGPackageDefinition >> traitNamed: traitName [
	"Retrieves an RGTraitDefinition object.
	traitName could be classSide name"
	| trait |

	^(trait:= self classOrTraitNamed: traitName) isTrait
	ifTrue:[ trait ]
	ifFalse:[ nil ]
]

{ #category : 'convenient accesses' }
RGPackageDefinition >> traitNames [
	"Retrieves the names of defined traits"

	^ self traits isDictionary
		ifTrue: [ self traits keys ]
		ifFalse:[ self traits collect:[ :trait| trait name ] ]
]

{ #category : 'convenient accesses' }
RGPackageDefinition >> traits [
	"Retrieves a collection (by default a dictionary) containing only defined traits"

	^self definedClasses select:[ :each| each isTrait ]
]

{ #category : 'iterating' }
RGPackageDefinition >> traitsDo: aBlock [

	self traits do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : 'adding-removing' }
RGPackageDefinition >> withoutPackages [

	self removeElementsCategorized: #packages
]
