"
A concrete container already knows how to treat classes, methods and packages. 
"
Class {
	#name : 'RGContainer',
	#superclass : 'RGDefinition',
	#instVars : [
		'elements'
	],
	#category : 'Ring-Definitions-Containers-Base',
	#package : 'Ring-Definitions-Containers',
	#tag : 'Base'
}

{ #category : 'class initialization' }
RGContainer class >> initialize [
	"Those got merged into RGPackageDefinition"

	self deprecatedAliases: { #RGAbstractContainer }
]

{ #category : 'adding-removing' }
RGContainer >> addClass: aRGBehaviorDefinition [
	"aRGBehaviorDefinition has to be a class, trait or metaclass"
	self flag: 'when i am adding a metaclass? check this?'.

	(aRGBehaviorDefinition isClass
		or:[ aRGBehaviorDefinition isTrait ]) ifFalse:[ ^self ].
	self addElement: aRGBehaviorDefinition in: self definedClasses
]

{ #category : 'adding-removing' }
RGContainer >> addClassNamed: className [
	"Creates a class with the given name"
	self addClass: (RGClassDefinition named: className)
]

{ #category : 'adding-removing' }
RGContainer >> addElement: anObject [

	anObject addInContainer: self
]

{ #category : 'adding-removing' }
RGContainer >> addElement: anObject in: aCollection [

	aCollection isDictionary
		ifTrue: [ aCollection at: anObject fullName put: anObject ]
		ifFalse:[ aCollection add: anObject ]
]

{ #category : 'adding-removing' }
RGContainer >> addMethod: aRGMethodDefinition [
	"aRGMethodDefinition needs to satisfy the status isMethod"

	aRGMethodDefinition isMethod ifFalse:[ ^self ].
	self addElement: aRGMethodDefinition in: self methods
]

{ #category : 'adding-removing' }
RGContainer >> addPackage: aRGPackage [
	"adds a child package"

	aRGPackage isPackage ifFalse:[ ^self ].
	self addElement: aRGPackage in: self packages
]

{ #category : 'adding-removing' }
RGContainer >> addPackageNamed: packageName [
	"adds a child package with the given name"

	(self includesPackageNamed: packageName) ifTrue: [ ^self ].
	self addPackage: (RGPackageDefinition named: packageName)
]

{ #category : 'adding-removing' }
RGContainer >> addTrait: aRGTraitDefinition [
	"convenient method"

	self addClass: aRGTraitDefinition
]

{ #category : 'adding-removing' }
RGContainer >> addTraitNamed: traitName [
	"Creates a trait with the given name"
	self addClass:  (RGTraitDefinition named: traitName)
]

{ #category : 'convenient accesses' }
RGContainer >> allClasses [
	"convenient method"

	^self classes
]

{ #category : 'iterating' }
RGContainer >> allClassesDo: aBlock [
	"Evaluate the argument, aBlock, for each class and its metaclass"

	self allClasses do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : 'convenient accesses' }
RGContainer >> allMethods [
	"convenient method"

	^self methods
]

{ #category : 'convenient accesses' }
RGContainer >> allTraits [
	"convenient method"

	^self allClasses select:[ :each | each isTrait ]
]

{ #category : 'iterating' }
RGContainer >> allTraitsDo: aBlock [
	"Evaluate the argument, aBlock, for each trait"

	self allTraits do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : 'lookup by name' }
RGContainer >> classNamed: className [
	"Retrieves an RGBehaviorDefinition object.
	className could be classSide name"

	^self classOrTraitNamed: className
]

{ #category : 'convenient accesses' }
RGContainer >> classNames [
	"Retrieves class names (including traits)"

	^ self definedClasses isDictionary
		ifTrue: [ self definedClasses keys ]
		ifFalse:[ self definedClasses collect:[ :class|  class name ] ]
]

{ #category : 'lookup by name' }
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

{ #category : 'convenient accesses' }
RGContainer >> classes [
	"convenient method"

	^self definedClasses
]

{ #category : 'convenient accesses' }
RGContainer >> classes: aCollection [
	"convenient method"

	self definedClasses: aCollection
]

{ #category : 'iterating' }
RGContainer >> classesDo: aBlock [

	self classes do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : 'image package loading' }
RGContainer >> cleanEmptyPackages [
	(self packages select: [ :pck | pck classes isEmpty and: [ pck methods isEmpty ] ]) do: [ :pck | self removePackage: pck ]
]

{ #category : 'accessing' }
RGContainer >> definedClasses [
	"Retrieves classes (traits are included)"

	^self elements at: #definedClasses ifAbsentPut:[ IdentityDictionary new ]
]

{ #category : 'accessing' }
RGContainer >> definedClasses: aCollection [
	"Set the classes collection"

	self elements at: #definedClasses put: aCollection
]

{ #category : 'iterating' }
RGContainer >> definedClassesDo: aBlock [

	self definedClasses do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : 'lookup elements' }
RGContainer >> elementNamed: elementName [

	| aSymbol |
	aSymbol := elementName asSymbol.
	elements do: [ :collection | (self elementNamed: aSymbol in: collection) ifNotNil: [ :found | ^ found ] ].
	^ nil
]

{ #category : 'lookup elements' }
RGContainer >> elementNamed: elementName in: aCollection [
	| aSymbol |

	aSymbol:= elementName asSymbol.
	^aCollection isDictionary
		ifTrue: [ aCollection at: aSymbol ifAbsent: [ nil ] ]
		ifFalse:[ aCollection detect:[ :each| each fullName = aSymbol ] ifNone:[ nil ] ]
]

{ #category : 'accessing' }
RGContainer >> elements [
	"Retrieves the elements"

	^elements
]

{ #category : 'managing elements groups' }
RGContainer >> elementsCategorized: aSymbol [
	"Retrieves a group of elements.
	If does not exist set an Set"

	^elements at: aSymbol ifAbsentPut:[ Set ]
]

{ #category : 'managing elements groups' }
RGContainer >> elementsCategorized: aSymbol with: aCollection [
	"Allows to define other groups of elements with a particular kind of collection"

	elements at: aSymbol put: aCollection
]

{ #category : 'iterating elements' }
RGContainer >> elementsDo: aBlock [
	elements valuesDo: [ :collection | collection do: [ :each | aBlock value: each ] ]
]

{ #category : 'convenient accesses' }
RGContainer >> extensionMethods [
	"Retrieves a collection (by default a dictionary) with the extensions methods of the receiver"

	^self methods values select:[ :mth| mth isExtension ]
]

{ #category : 'image package loading' }
RGContainer >> findPackageOfClass: aRGBehaviorDefinition using: packageKeys [
	"Look for the package of a class. It is nil when there is not a package created for a category in MC"

	self packages isEmpty ifTrue: [ ^ nil ].
	^ self packages at: aRGBehaviorDefinition category ifAbsent: [
		  packageKeys
			  detect: [ :pair | aRGBehaviorDefinition category isCategoryOf: pair key ]
			  ifFound: [ :pair | self packages at: pair key ]
			  ifNone: [ nil ] ]
]

{ #category : 'image package loading' }
RGContainer >> findPackageOfMethod: aRGMethodDefinition using: packageKeys [
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
RGContainer >> flushElements [

	elements:= IdentityDictionary new
]

{ #category : 'testing' }
RGContainer >> hasClasses [

	^self definedClasses notEmpty
]

{ #category : 'testing' }
RGContainer >> hasMethods [

	^self methods notEmpty
]

{ #category : 'testing' }
RGContainer >> includesClass: aRGBehaviorDefinition [
	"Returns true if the receiver includes aRGBehaviorDefinition in the defined classes"

	^self definedClasses includes: aRGBehaviorDefinition
]

{ #category : 'testing' }
RGContainer >> includesClassNamed: className [
	"Returns true if the receiver includes the className in the defined classes"

	self flag: 'can a metaclass be stored without its nonMetaClass? Check this'.
	^self includesElementNamed: (self theNonMetaClassNameOf: className) in: self definedClasses
]

{ #category : 'testing' }
RGContainer >> includesElement: anRGDefinition [

	^anRGDefinition isIncludedInContainer: self
]

{ #category : 'testing' }
RGContainer >> includesElementNamed: elementName [

	^ elements anySatisfy: [ :collection | self includesElementNamed: elementName asSymbol in: collection ]
]

{ #category : 'testing' }
RGContainer >> includesElementNamed: elementName in: aCollection [

	^aCollection isDictionary
		ifTrue: [ aCollection includesKey:  elementName asSymbol ]
		ifFalse:[ aCollection anySatisfy: [ :each| each name = elementName asSymbol ] ]
]

{ #category : 'testing' }
RGContainer >> includesMethod: aRGMethodDefinition [

	^self methods includes: aRGMethodDefinition
]

{ #category : 'testing' }
RGContainer >> includesMethodNamed: fullSelectorName [

	^self includesElementNamed: fullSelectorName in: self methods
]

{ #category : 'testing' }
RGContainer >> includesPackage: aRGPackage [
	"Verifies if a child package == aRGPackage exists"

	^self packages includes: aRGPackage
]

{ #category : 'testing' }
RGContainer >> includesPackageNamed: packageName [
	"Verifies if a child package with name = packageName exists"

	^self includesElementNamed: packageName in: self packages
]

{ #category : 'initialization' }
RGContainer >> initialize [

	super initialize.
	elements := IdentityDictionary new.
	self definedClasses: IdentityDictionary new.
	self methods: IdentityDictionary new.
	self packages: IdentityDictionary new
]

{ #category : 'utilities' }
RGContainer >> isMetaclassName: aSymbol [
	"Validates if the arguments corresponds to a nonMetaClass"

	^(aSymbol
		indexOfSubCollection: ' class'
		startingAt: 1) > 0
]

{ #category : 'image class and method loading' }
RGContainer >> loadClass: aRGBehaviorDefinition using: packageKeys [

	self setPackageOfClass: aRGBehaviorDefinition using: packageKeys.
	self addClass: aRGBehaviorDefinition
]

{ #category : 'image class and method loading' }
RGContainer >> loadMethod: aRGMethodDefinition inClass: aRGBehaviorDefinition using: packageKeys [

	self addMethod: aRGMethodDefinition.
	aRGBehaviorDefinition addMethod: aRGMethodDefinition.
	self setPackageOfMethod: aRGMethodDefinition ofClass: aRGBehaviorDefinition using: packageKeys
]

{ #category : 'image package loading' }
RGContainer >> loadPackagesFromImage [

	| rgPackage rgPackageKeys |
	rgPackageKeys := OrderedCollection new.
	self class allWorkingCopies do: [ :pck |
		rgPackage := RGPackageDefinition named: pck package name asSymbol.
		rgPackageKeys add: rgPackage name -> ('*' , rgPackage name asLowercase).
		self addPackage: rgPackage ].
	^ rgPackageKeys
]

{ #category : 'image class and method loading' }
RGContainer >> loadTraitUsers [
	"Set the users of a trait"

	(self classes select: [ :class | class hasTraitComposition ]) do: [ :user |
		user traitNames do: [ :tname |
			(self traitNamed: tname) ifNotNil: [ :rgTrait |
				rgTrait addUser: user.
				rgTrait classSide addUser: user classSide ] ] ]
]

{ #category : 'lookup by name' }
RGContainer >> metaclassNamed: metaclassName [

	(self classOrTraitNamed: metaclassName) ifNotNil: [ :theClass |
		^ theClass isMeta
			  ifTrue: [ theClass ]
			  ifFalse: [ theClass classSide ] ].

	^ nil
]

{ #category : 'lookup by name' }
RGContainer >> methodNamed: fullSelectorName [

	^self elementNamed: fullSelectorName in: self methods
]

{ #category : 'accessing' }
RGContainer >> methods [
	"Retrieves methods defined in the container"

	^self elements at: #methods ifAbsentPut:[ IdentityDictionary new ]
]

{ #category : 'accessing' }
RGContainer >> methods: aCollection [
	"Set the methods collection"

	self elements at: #methods put: aCollection
]

{ #category : 'iterating' }
RGContainer >> methodsDo: aBlock [

	self methods
		do:[ :each| aBlock value: each ]
]

{ #category : 'lookup by name' }
RGContainer >> packageNamed: packageName [
	"Look for a child package named packageName"

	^self elementNamed: packageName in: self packages
]

{ #category : 'accessing' }
RGContainer >> packages [
	"Retrieves the children packages defined in the receiver"

	^self elements at: #packages ifAbsentPut:[ IdentityDictionary new ]
]

{ #category : 'accessing' }
RGContainer >> packages: aCollection [

	self elementsCategorized: #packages with: aCollection
]

{ #category : 'adding-removing' }
RGContainer >> removeClass: aRGAbstractClassDefinition [

	self removeElement: aRGAbstractClassDefinition from: self definedClasses
]

{ #category : 'adding-removing' }
RGContainer >> removeElement: anObject [

	anObject removeFromContainer: self
]

{ #category : 'adding-removing' }
RGContainer >> removeElement: anObject from: aCollection [

	aCollection isDictionary
		ifTrue: [ aCollection removeKey: anObject fullName ifAbsent:[ ] ]
		ifFalse:[ aCollection remove: anObject ifAbsent:[ ] ]
]

{ #category : 'managing elements groups' }
RGContainer >> removeElementsCategorized: aSymbol [
	"Deletes a group of elements"

	elements removeKey: aSymbol ifAbsent:[ ]
]

{ #category : 'adding-removing' }
RGContainer >> removeMethod: aRGMethodDefinition [

	self removeElement: aRGMethodDefinition from: self methods
]

{ #category : 'adding-removing' }
RGContainer >> removePackage: aRGPackage [
	"removes a child aRGPackage"

	aRGPackage isPackage ifFalse:[ ^self ].
	self removeElement: aRGPackage from: self packages
]

{ #category : 'adding-removing' }
RGContainer >> removeTrait: aRGTraitDefinition [
	"convenient method"

	self removeClass: aRGTraitDefinition
]

{ #category : 'image package loading' }
RGContainer >> setPackageOfClass: rgClass using: packageKeys [
	"Set the package to aRGBehaviorDefinition"

	(self findPackageOfClass: rgClass using: packageKeys)
		ifNotNil:[ :pck| pck addClass: rgClass ]
]

{ #category : 'image package loading' }
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

{ #category : 'utilities' }
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

{ #category : 'lookup by name' }
RGContainer >> traitNamed: traitName [
	"Retrieves an RGTraitDefinition object.
	traitName could be classSide name"
	| trait |

	^(trait:= self classOrTraitNamed: traitName) isTrait
	ifTrue:[ trait ]
	ifFalse:[ nil ]
]

{ #category : 'convenient accesses' }
RGContainer >> traitNames [
	"Retrieves the names of defined traits"

	^ self traits isDictionary
		ifTrue: [ self traits keys ]
		ifFalse:[ self traits collect:[ :trait| trait name ] ]
]

{ #category : 'convenient accesses' }
RGContainer >> traits [
	"Retrieves a collection (by default a dictionary) containing only defined traits"

	^self definedClasses select:[ :each| each isTrait ]
]

{ #category : 'iterating' }
RGContainer >> traitsDo: aBlock [

	self traits do: [ :each|
		aBlock value: each.
		each hasMetaclass
			ifTrue:[ aBlock value: each classSide ] ]
]

{ #category : 'adding-removing' }
RGContainer >> withoutClasses [

	self removeElementsCategorized: #definedClasses
]

{ #category : 'adding-removing' }
RGContainer >> withoutMethods [

	self removeElementsCategorized: #methods
]

{ #category : 'adding-removing' }
RGContainer >> withoutPackages [

	self removeElementsCategorized: #packages
]
