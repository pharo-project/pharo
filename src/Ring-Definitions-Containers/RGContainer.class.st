"
A concrete container already knows how to treat classes, methods and packages. 
"
Class {
	#name : #RGContainer,
	#superclass : #RGAbstractContainer,
	#category : #'Ring-Definitions-Containers-Base'
}

{ #category : #'image package loading' }
RGContainer class >> allManagers [

	^ Smalltalk globals at: #MCWorkingCopy 
		ifPresent: [:mcwc | mcwc allManagers ] 
		ifAbsent: [OrderedCollection new ]

]

{ #category : #'image package loading' }
RGContainer class >> category: categoryName matches: prefix [

	| prefixSize catSize |
	categoryName ifNil: [ ^false ].
	catSize := categoryName size.
	prefixSize := prefix size.
	catSize < prefixSize ifTrue: [ ^false ].
	(categoryName findString: prefix startingAt: 1 caseSensitive: true) = 1
		ifFalse: [ ^false ].
	^(categoryName at: prefix size + 1 ifAbsent: [ ^true ]) = $-
]

{ #category : #'image package loading' }
RGContainer class >> packageKeys [

	^ self allManagers collect: [ :pck | pck package name asSymbol -> ('*', pck package name asLowercase) ]
]

{ #category : #'image package loading' }
RGContainer class >> packageNames [

	^ self allManagers collect: [ :pck | pck package name asSymbol ]
]

{ #category : #'image package loading' }
RGContainer class >> packageOfClass: aRGBehaviorDefinition [

	^ self packageOfClass: aRGBehaviorDefinition using: self packageNames.
]

{ #category : #'image package loading' }
RGContainer class >> packageOfClass: aRGBehaviorDefinition using: packageNames [

	"Looks for the package of aRGBehaviorDefinition from the image"
	| pName |
	aRGBehaviorDefinition ifNil:[ ^nil ].
	pName := (packageNames 
				detect: [ :each| each = aRGBehaviorDefinition category ]
				ifNone: [ packageNames detect:[ :each| self category: aRGBehaviorDefinition category matches: each ] ifNone:[ nil ] ]).
	^ pName ifNotNil:[ RGPackageDefinition named: pName ]
]

{ #category : #'image package loading' }
RGContainer class >> packageOfMethod: aRGMethodDefinition [

	^ self packageOfMethod: aRGMethodDefinition using: self packageKeys
]

{ #category : #'image package loading' }
RGContainer class >> packageOfMethod: aRGMethodDefinition using: packageKeys [

	"Looks for the package of aRGMethodDefinition from the image"
	| pName parentPackage |
	(aRGMethodDefinition protocol notNil and:[ aRGMethodDefinition protocol beginsWith: '*' ]) ifFalse:[
		parentPackage := (aRGMethodDefinition parent ifNotNil:[ aRGMethodDefinition parent package ]).	
		^ parentPackage ifNil:[ self packageOfClass: aRGMethodDefinition parent ] ].

	aRGMethodDefinition protocol ifNil: [ ^ nil ].
	pName := (packageKeys 
				detect: [ :each| self category: aRGMethodDefinition protocol asLowercase matches: each value ] ifNone:[ nil ]).
	^ pName ifNotNil:[ RGPackageDefinition named: pName key ]
]

{ #category : #'adding/removing' }
RGContainer >> addClass: aRGBehaviorDefinition [
	"aRGBehaviorDefinition has to be a class, trait or metaclass"
	self flag: 'when i am adding a metaclass? check this?'.

	(aRGBehaviorDefinition isClass
		or:[ aRGBehaviorDefinition isTrait ]) ifFalse:[ ^self ].
	self addElement: aRGBehaviorDefinition in: self definedClasses
]

{ #category : #'adding/removing' }
RGContainer >> addClassNamed: className [ 
	"Creates a class with the given name"
	self addClass: (RGClassDefinition named: className)
]

{ #category : #'adding/removing' }
RGContainer >> addMethod: aRGMethodDefinition [
	"aRGMethodDefinition needs to satisfy the status isMethod"

	aRGMethodDefinition isMethod ifFalse:[ ^self ].
	self addElement: aRGMethodDefinition in: self methods
]

{ #category : #'adding/removing' }
RGContainer >> addPackage: aRGPackage [
	"adds a child package"

	aRGPackage isPackage ifFalse:[ ^self ].
	self addElement: aRGPackage in: self packages
]

{ #category : #'adding/removing' }
RGContainer >> addPackageNamed: packageName [
	"adds a child package with the given name"

	(self includesPackageNamed: packageName) ifTrue: [ ^self ].
	self addPackage: (RGPackageDefinition named: packageName)
]

{ #category : #'adding/removing' }
RGContainer >> addTrait: aRGTraitDefinition [
	"convenient method"

	self addClass: aRGTraitDefinition
]

{ #category : #'adding/removing' }
RGContainer >> addTraitNamed: traitName [ 
	"Creates a trait with the given name"
	self addClass:  (RGTraitDefinition named: traitName)
]

{ #category : #'convenient accesses' }
RGContainer >> allClasses [
	"convenient method"

	^self classes
]

{ #category : #iterating }
RGContainer >> allClassesDo: aBlock [
	"Evaluate the argument, aBlock, for each class and its metaclass"

	self allClasses do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : #'convenient accesses' }
RGContainer >> allMethods [
	"convenient method"

	^self methods
]

{ #category : #'convenient accesses' }
RGContainer >> allTraits [
	"convenient method"
	
	^self allClasses select:[ :each | each isTrait ]
]

{ #category : #iterating }
RGContainer >> allTraitsDo: aBlock [
	"Evaluate the argument, aBlock, for each trait"

	self allTraits do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : #'lookup by name' }
RGContainer >> classNamed: className [
	"Retrieves an RGBehaviorDefinition object.
	className could be theMetaClass name"
	
	^self classOrTraitNamed: className
]

{ #category : #'convenient accesses' }
RGContainer >> classNames [
	"Retrieves class names (including traits)"
	
	^ self definedClasses isDictionary
		ifTrue: [ self definedClasses keys ]
		ifFalse:[ self definedClasses collect:[ :class|  class name ] ]
]

{ #category : #'lookup by name' }
RGContainer >> classOrTraitNamed: className [
	"A class or metaclass can be reached by its name"
	| theClass |

	className ifNil: [^nil ].
	theClass:= self elementNamed: (self theNonMetaClassNameOf: className) in: self definedClasses.
	theClass ifNil:[ ^nil ].
	
	^(self isMetaclassName: className)
		ifTrue: [ theClass classSide ]
		ifFalse:[ theClass ] 
]

{ #category : #'convenient accesses' }
RGContainer >> classes [
	"convenient method"

	^self definedClasses
]

{ #category : #'convenient accesses' }
RGContainer >> classes: aCollection [
	"convenient method"

	self definedClasses: aCollection
]

{ #category : #iterating }
RGContainer >> classesDo: aBlock [

	self classes do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : #'image package loading' }
RGContainer >> cleanEmptyPackages [
	(self packages select: [ :pck | pck classes isEmpty and: [ pck methods isEmpty ] ]) do: [ :pck | self removePackage: pck ]
]

{ #category : #accessing }
RGContainer >> definedClasses [
	"Retrieves classes (traits are included)"

	^self elements at: #definedClasses ifAbsentPut:[ IdentityDictionary new ]
]

{ #category : #accessing }
RGContainer >> definedClasses: aCollection [
	"Set the classes collection"

	self elements at: #definedClasses put: aCollection
]

{ #category : #iterating }
RGContainer >> definedClassesDo: aBlock [

	self definedClasses do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : #'convenient accesses' }
RGContainer >> extensionMethods [
	"Retrieves a collection (by default a dictionary) with the extensions methods of the receiver"

	^self methods values select:[ :mth| mth isExtension ]
]

{ #category : #'image package loading' }
RGContainer >> findPackageOfClass: aRGBehaviorDefinition using: packageKeys [

	"Look for the package of a class. It is nil when there is not a package created for a category in MC"
	| pair |
	self packages isEmpty ifTrue: [ ^ nil ].
	^ self packages at: aRGBehaviorDefinition category ifAbsent:[
		pair := packageKeys detect:[ :each| self class category: aRGBehaviorDefinition category matches: each key ] ifNone:[ nil ].
		pair ifNotNil:[ self packages at: pair key ] ]
]

{ #category : #'image package loading' }
RGContainer >> findPackageOfMethod: aRGMethodDefinition using: packageKeys [

	"Look for the package of an extension method. nil otherwise"
	| pair lname |	
	self packages isEmpty ifTrue: [ ^nil ].
	(aRGMethodDefinition protocol beginsWith: '*') ifFalse:[ ^ nil ].
	lname := aRGMethodDefinition protocol asLowercase.
 	pair := packageKeys detect:[ :assoc| self class category: lname matches: assoc value ] ifNone:[ nil ].
	^ pair ifNotNil:[ self packages at: pair key ]
]

{ #category : #testing }
RGContainer >> hasClasses [

	^self definedClasses notEmpty
]

{ #category : #testing }
RGContainer >> hasMethods [

	^self methods notEmpty
]

{ #category : #testing }
RGContainer >> includesClass: aRGBehaviorDefinition [
	"Returns true if the receiver includes aRGBehaviorDefinition in the defined classes"

	^self definedClasses includes: aRGBehaviorDefinition
]

{ #category : #testing }
RGContainer >> includesClassNamed: className [
	"Returns true if the receiver includes the className in the defined classes"

	self flag: 'can a metaclass be stored without its nonMetaClass? Check this'.
	^self includesElementNamed: (self theNonMetaClassNameOf: className) in: self definedClasses.
]

{ #category : #testing }
RGContainer >> includesMethod: aRGMethodDefinition [

	^self methods includes: aRGMethodDefinition
]

{ #category : #testing }
RGContainer >> includesMethodNamed: fullSelectorName [
	
	^self includesElementNamed: fullSelectorName in: self methods
]

{ #category : #testing }
RGContainer >> includesPackage: aRGPackage [
	"Verifies if a child package == aRGPackage exists"
	
	^self packages includes: aRGPackage
]

{ #category : #testing }
RGContainer >> includesPackageNamed: packageName [
	"Verifies if a child package with name = packageName exists"
	
	^self includesElementNamed: packageName in: self packages
]

{ #category : #initialization }
RGContainer >> initialize [

	super initialize.
	self definedClasses: IdentityDictionary new.
	self methods: IdentityDictionary new.
	self packages: IdentityDictionary new.
]

{ #category : #utilities }
RGContainer >> isMetaclassName: aSymbol [
	"Validates if the arguments corresponds to a nonMetaClass"
	
	^(aSymbol
		indexOfSubCollection: ' class'
		startingAt: 1) > 0
]

{ #category : #'image class and method loading' }
RGContainer >> loadClass: aRGBehaviorDefinition using: packageKeys [

	self setPackageOfClass: aRGBehaviorDefinition using: packageKeys.
	self addClass: aRGBehaviorDefinition
]

{ #category : #'image class and method loading' }
RGContainer >> loadMethod: aRGMethodDefinition inClass: aRGBehaviorDefinition using: packageKeys [

	self addMethod: aRGMethodDefinition.
	aRGBehaviorDefinition addMethod: aRGMethodDefinition.
	self setPackageOfMethod: aRGMethodDefinition ofClass: aRGBehaviorDefinition using: packageKeys
]

{ #category : #'image package loading' }
RGContainer >> loadPackagesFromImage [

	| rgPackage rgPackageKeys |
	rgPackageKeys := OrderedCollection new.
	self class allManagers do: [ :pck | 
		rgPackage := RGPackageDefinition named: pck package name asSymbol.
		rgPackageKeys add: (rgPackage name -> ('*', rgPackage name asLowercase)).
		self addPackage: rgPackage ].
	^ rgPackageKeys
]

{ #category : #'image class and method loading' }
RGContainer >> loadTraitUsers [
	"Set the users of a trait"
	| users |
	
	users := self classes select: [ :cls | cls hasTraitComposition ].
	users do:[ :each |  | rgTrait |
		each traitNames do:[ :tname|
			rgTrait := self traitNamed: tname.
			rgTrait notNil ifTrue: [
				rgTrait addUser: each.
				rgTrait classSide addUser: each classSide ] ] ]
]

{ #category : #'lookup by name' }
RGContainer >> metaclassNamed: metaclassName [
	| theClass |
	
	theClass:= (self classOrTraitNamed: metaclassName).
	theClass notNil
	ifTrue:[ ^theClass isMeta 
		      ifTrue: [ theClass ]
		      ifFalse:[ theClass classSide ] ].
	
	^nil
]

{ #category : #'lookup by name' }
RGContainer >> methodNamed: fullSelectorName [

	^self elementNamed: fullSelectorName in: self methods
]

{ #category : #accessing }
RGContainer >> methods [
	"Retrieves methods defined in the container"
	
	^self elements at: #methods ifAbsentPut:[ IdentityDictionary new ]
]

{ #category : #accessing }
RGContainer >> methods: aCollection [
	"Set the methods collection"
	
	self elements at: #methods put: aCollection
]

{ #category : #iterating }
RGContainer >> methodsDo: aBlock [

	self methods
		do:[ :each| aBlock value: each ]
]

{ #category : #'lookup by name' }
RGContainer >> packageNamed: packageName [
	"Look for a child package named packageName"
	
	^self elementNamed: packageName in: self packages
]

{ #category : #accessing }
RGContainer >> packages [
	"Retrieves the children packages defined in the receiver"

	^self elements at: #packages ifAbsentPut:[ IdentityDictionary new ]
]

{ #category : #accessing }
RGContainer >> packages: aCollection [

	self elementsCategorized: #packages with: aCollection
]

{ #category : #'adding/removing' }
RGContainer >> removeClass: aRGAbstractClassDefinition [

	self removeElement: aRGAbstractClassDefinition from: self definedClasses
]

{ #category : #'adding/removing' }
RGContainer >> removeMethod: aRGMethodDefinition [

	self removeElement: aRGMethodDefinition from: self methods
]

{ #category : #'adding/removing' }
RGContainer >> removePackage: aRGPackage [
	"removes a child aRGPackage"

	aRGPackage isPackage ifFalse:[ ^self ].
	self removeElement: aRGPackage from: self packages
]

{ #category : #'adding/removing' }
RGContainer >> removeTrait: aRGTraitDefinition [
	"convenient method"

	self removeClass: aRGTraitDefinition
]

{ #category : #'image package loading' }
RGContainer >> setPackageOfClass: rgClass using: packageKeys [
	"Set the package to aRGBehaviorDefinition"

	(self findPackageOfClass: rgClass using: packageKeys) 
		ifNotNil:[ :pck| pck addClass: rgClass ]
	
]

{ #category : #'image package loading' }
RGContainer >> setPackageOfMethod: rgMethod ofClass: rgClass using: packageKeys [
	"Set the package to aRGMethodDefinition and its value isExtension"

	(self findPackageOfMethod: rgMethod using: packageKeys) 
		ifNil:[ rgClass package ifNotNil:[ :pck| 
				pck addMethod: rgMethod.
				rgMethod isExtension: false ] ]
		ifNotNil:[ :pck| 
			pck addMethod: rgMethod.
			rgMethod isExtension: (rgMethod package ~= rgClass package) ]
]

{ #category : #utilities }
RGContainer >> theNonMetaClassNameOf: aSymbol [
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

{ #category : #'lookup by name' }
RGContainer >> traitNamed: traitName [
	"Retrieves an RGTraitDefinition object.
	traitName could be theMetaClass name"
	| trait |
	
	^(trait:= self classOrTraitNamed: traitName) isTrait
	ifTrue:[ trait ]
	ifFalse:[ nil ]
]

{ #category : #'convenient accesses' }
RGContainer >> traitNames [
	"Retrieves the names of defined traits"

	^ self traits isDictionary
		ifTrue: [ self traits keys ]
		ifFalse:[ self traits collect:[ :trait| trait name ] ]
]

{ #category : #'convenient accesses' }
RGContainer >> traits [
	"Retrieves a collection (by default a dictionary) containing only defined traits"

	^self definedClasses select:[ :each| each isTrait ]
]

{ #category : #iterating }
RGContainer >> traitsDo: aBlock [

	self traits do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : #'adding/removing' }
RGContainer >> withoutClasses [

	self removeElementsCategorized: #definedClasses
]

{ #category : #'adding/removing' }
RGContainer >> withoutMethods [
	
	self removeElementsCategorized: #methods
]

{ #category : #'adding/removing' }
RGContainer >> withoutPackages [
	
	self removeElementsCategorized: #packages
]
