"
RGPackage is the representation for packages.
A package manages classes and methods as elements
"
Class {
	#name : 'RGPackageDefinition',
	#superclass : 'RGDefinition',
	#instVars : [
		'definedClasses',
		'definedMethods',
		'extensionMethods'
	],
	#category : 'Ring-Definitions-Core-Base',
	#package : 'Ring-Definitions-Core',
	#tag : 'Base'
}

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
RGPackageDefinition >> addMethod: aRGMethodDefinition [
	"aRGMethodDefinition needs to satisfy the status isMethod"

	aRGMethodDefinition isMethod ifFalse: [ ^ self ].
	aRGMethodDefinition package: self. "Needed to evaluate #isExtension if it was not set in advance"
	(aRGMethodDefinition isExtension
		 ifTrue: [ self extensionMethods ]
		 ifFalse: [ self definedMethods ]) at: aRGMethodDefinition fullName put: aRGMethodDefinition
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

	^ self classes
]

{ #category : 'iterating' }
RGPackageDefinition >> allClassesDo: aBlock [
	"Evaluate the argument, aBlock, for each class and its metaclass"

	self allClasses do: [ :class |
		aBlock value: class.
		class hasMetaclass ifTrue: [ aBlock value: class classSide ] ]
]

{ #category : 'convenient accesses' }
RGPackageDefinition >> allMethods [
	"convenient method"

	^ self methods
]

{ #category : 'convenient accesses' }
RGPackageDefinition >> allTraits [
	"convenient method"

	^ self allClasses select: [ :behavior | behavior isTrait ]
]

{ #category : 'iterating' }
RGPackageDefinition >> allTraitsDo: aBlock [
	"Evaluate the argument, aBlock, for each trait"

	self allTraits do: [ :trait |
		aBlock value: trait.
		trait hasMetaclass ifTrue: [ aBlock value: trait classSide ] ]
]

{ #category : 'lookup by name' }
RGPackageDefinition >> classNamed: className [
	"Retrieves an RGBehaviorDefinition object.
	className could be classSide name"

	^ self classOrTraitNamed: className
]

{ #category : 'accessing' }
RGPackageDefinition >> classNames [

	^ self classes keys
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

{ #category : 'iterating' }
RGPackageDefinition >> classesDo: aBlock [

	self classes do: [ :class |
		aBlock value: class.
		class hasMetaclass ifTrue: [ aBlock value: class classSide ] ]
]

{ #category : 'convenient accesses' }
RGPackageDefinition >> definedClassNames [
	"Retrieves class names (including traits)"

	^ self definedClasses keys
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

	self definedClasses do: [ :class |
		aBlock value: class.
		class hasMetaclass ifTrue: [ aBlock value: class classSide ] ]
]

{ #category : 'accessing' }
RGPackageDefinition >> definedMethods [
	"Retrieves methods defined in the container"

	^ definedMethods
]

{ #category : 'accessing' }
RGPackageDefinition >> definedTraits [
	"Retrieves the traits defined in the receiver"

	^ self traits
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

	^ (self definesClass: aRGBehaviorDefinition) or: [ self extendsClass: aRGBehaviorDefinition ]
]

{ #category : 'accessing' }
RGPackageDefinition >> extendedClassNames [

	^ self extensionMethods collect: [ :method | method parentName ] as: Set
]

{ #category : 'accessing' }
RGPackageDefinition >> extendedClasses [
	"Retrieves the classes holding the extension methods and returns a dictionary as well"

	| classesOfExt |
	classesOfExt := Dictionary new.

	self extensionMethods
		reject: [ :method | classesOfExt includesKey: method parentName ]
		thenDo: [ :method | classesOfExt at: method parentName put: method parent ].

	^ classesOfExt
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

	^ extensionMethods
]

{ #category : 'accessing' }
RGPackageDefinition >> extensionMethods: aCollection [
	"Set the extension methods collection"

	extensionMethods := aCollection
]

{ #category : 'testing' }
RGPackageDefinition >> hasClasses [

	^ self definedClasses isNotEmpty
]

{ #category : 'testing' }
RGPackageDefinition >> hasMethods [

	^ self methods isNotEmpty
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
	^ self definedClasses includesKey: (self theNonMetaClassNameOf: className)
]

{ #category : 'testing' }
RGPackageDefinition >> includesMethod: aRGMethodDefinition [

	^ self methods includes: aRGMethodDefinition
]

{ #category : 'testing' }
RGPackageDefinition >> includesMethodNamed: fullSelectorName [

	^ self methods includesKey: fullSelectorName
]

{ #category : 'initialization' }
RGPackageDefinition >> initialize [

	super initialize.
	definedClasses := IdentityDictionary new.
	definedMethods := IdentityDictionary new.
	extensionMethods := IdentityDictionary new
]

{ #category : 'utilities' }
RGPackageDefinition >> isMetaclassName: aSymbol [
	"Validates if the arguments corresponds to a nonMetaClass"

	^ (aSymbol indexOfSubCollection: ' class' startingAt: 1) > 0
]

{ #category : 'testing' }
RGPackageDefinition >> isPackage [

	^ true
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

	^ self methods at: fullSelectorName asSymbol ifAbsent: [ nil ]
]

{ #category : 'accessing' }
RGPackageDefinition >> methods [
	"Retrieves all the methods defined in the receiver."

	^ self definedMethods , self extensionMethods
]

{ #category : 'iterating' }
RGPackageDefinition >> methodsDo: aBlock [

	self methods do: [ :each | aBlock value: each ]
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
RGPackageDefinition >> parent [
	"Retrieves the environment linked to this package"

	^ self environment
]

{ #category : 'printing' }
RGPackageDefinition >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: self name;
		nextPut: $)
]

{ #category : 'accessing' }
RGPackageDefinition >> realPackage [

	^ PackageOrganizer default packageNamed: self name
]

{ #category : 'adding-removing' }
RGPackageDefinition >> removeClass: aRGAbstractClassDefinition [

	self definedClasses removeKey: aRGAbstractClassDefinition fullName ifAbsent: [  ]
]

{ #category : 'adding-removing' }
RGPackageDefinition >> removeMethod: aRGMethodDefinition [

	(aRGMethodDefinition isExtension
		 ifTrue: [ self extensionMethods ]
		 ifFalse: [ self definedMethods ]) removeKey: aRGMethodDefinition fullName ifAbsent: [  ]
]

{ #category : 'adding-removing' }
RGPackageDefinition >> removeTrait: aRGTraitDefinition [
	"convenient method"

	self removeClass: aRGTraitDefinition
]

{ #category : 'utilities' }
RGPackageDefinition >> theNonMetaClassNameOf: aSymbol [
	"Rejects the prefix ' class' or ' classTrait' of the argument"

	| index |
	index := aSymbol indexOfSubCollection: ' class' startingAt: 1 ifAbsent: [ ^ aSymbol asSymbol ].

	^ (aSymbol copyFrom: 1 to: index - 1) asSymbol
]

{ #category : 'lookup by name' }
RGPackageDefinition >> traitNamed: traitName [
	"Retrieves an RGTraitDefinition object.
	traitName could be classSide name"

	| trait |
	^ (trait := self classOrTraitNamed: traitName) isTrait
		  ifTrue: [ trait ]
		  ifFalse: [ nil ]
]

{ #category : 'convenient accesses' }
RGPackageDefinition >> traitNames [
	"Retrieves the names of defined traits"

	^ self traits keys
]

{ #category : 'convenient accesses' }
RGPackageDefinition >> traits [
	"Retrieves a collection (by default a dictionary) containing only defined traits"

	^ self definedClasses select: [ :behavior | behavior isTrait ]
]

{ #category : 'iterating' }
RGPackageDefinition >> traitsDo: aBlock [

	self traits do: [ :trait |
		aBlock value: trait.
		trait hasMetaclass ifTrue: [ aBlock value: trait classSide ] ]
]
