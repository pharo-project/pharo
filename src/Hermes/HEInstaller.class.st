Class {
	#name : #HEInstaller,
	#superclass : #Object,
	#instVars : [
		'environment',
		'originalUndeclareds'
	],
	#category : #'Hermes-ClassInstaller'
}

{ #category : #converting }
HEInstaller >> asClassVariables: aString [
	| trimmed |
	trimmed := aString trimBoth.

	trimmed isEmpty
		ifTrue: [ ^ #() ].

	^ (trimmed substrings: ' ') collect: [:x | x asSymbol => ClassVariable]
]

{ #category : #converting }
HEInstaller >> asSlots: aString [
	| trimmed |
	trimmed := aString trimBoth.

	trimmed isEmpty
		ifTrue: [ ^ #() ].

	^ (trimmed splitOn: ' ') collect: [ :e | e asSymbol asSlot ] as: Array
]

{ #category : #'creating classes' }
HEInstaller >> build: aHEClass [
	| newMetaclass newClass superclass layoutType |

	(self existingClass: aHEClass) ifNotNil: [ :x | ^ x ].

	newMetaclass := Metaclass new.
	
	superclass := self classNamed: aHEClass superclass.
	layoutType := self classNamed: aHEClass layoutClass.

	newMetaclass 
		superclass: (superclass ifNil: [Class] ifNotNil: [superclass class]) 
		withLayoutType: FixedLayout 
		slots: (self asSlots: aHEClass classInstancevariables).
	
	newClass := newMetaclass new.
	newClass setName: aHEClass className.
	
	newClass superclass: superclass withLayoutType: layoutType slots: (self asSlots: aHEClass instanceVariables).
	newClass environment: environment.	
	newClass basicCategory: aHEClass category.
	
	newClass
		declareClassVariables: (self asClassVariables:aHEClass classVariables);
		sharing: aHEClass sharedPools.	

	newClass classLayout slots do: [ :each | each installingIn: newClass ].
	newClass classLayout slots do: [ :each | each layoutChanged: newClass ].

	newClass traitComposition: (aHEClass traitComposition asLiteralIn: environment).
	newClass class traitComposition: (aHEClass classTraitComposition asLiteralIn: environment).
	
	newClass applyChangesOfNewTraitCompositionReplacing:#() asTraitComposition.
	newClass class applyChangesOfNewTraitCompositionReplacing:#() asTraitComposition.
	
	^newClass.

]

{ #category : #'creating traits' }
HEInstaller >> buildTrait: aTraitDefinition [
	| newTrait traitComposition |
	
	(self existingTrait: aTraitDefinition) ifNotNil: [:x | ^ x ].
	
	traitComposition := aTraitDefinition traitComposition asLiteralIn: environment.
	
	newTrait := Trait
		named: aTraitDefinition traitName
		uses: traitComposition
		category: aTraitDefinition category
		env: environment.

	newTrait classTrait traitComposition: (aTraitDefinition classTraitComposition asLiteralIn: environment).

	^ newTrait
]

{ #category : #accessing }
HEInstaller >> classNamed: aSymbol [
	aSymbol ifNil: [ ^ nil ].
	^ self environment at: aSymbol
]

{ #category : #'reporting undeclared' }
HEInstaller >> createdUndeclared [
	^ (Undeclared associations reject: [ :asoc | originalUndeclareds associations includes: asoc]) asDictionary 
]

{ #category : #accessing }
HEInstaller >> environment [
	^ environment
]

{ #category : #accessing }
HEInstaller >> environment: anObject [
	environment := anObject
]

{ #category : #'validating existance' }
HEInstaller >> existingClass: aHEClass [
	(environment hasClassNamed: aHEClass className) ifTrue:[
		self error: (self messageExistingClass: aHEClass)
	].
	^ nil
]

{ #category : #'validating existance' }
HEInstaller >> existingTrait: aHETrait. [
	(environment includesKey: aHETrait traitName) ifTrue:[ 
		self error: (self messageExistingTrait: aHETrait)
	].
	
	^nil
]

{ #category : #initialization }
HEInstaller >> initialize [
	environment := self class environment.
	originalUndeclareds := Undeclared copy.

]

{ #category : #'creating classes' }
HEInstaller >> installClass: aClass in: category [
	environment at: aClass name put: aClass.
	aClass superclass addSubclass: aClass.

	aClass basicCategory: category.
   environment organization ifNotNil: [ :e | e classify: aClass name under: category ].
	SystemAnnouncer uniqueInstance classAdded: aClass inCategory: category.

]

{ #category : #'installing methods' }
HEInstaller >> installExtensionMethod: extensionMethod [
	| aClass |
	aClass := self environment classNamed: extensionMethod className.
	self rebuildMethod: extensionMethod into: aClass.
]

{ #category : #'installing methods' }
HEInstaller >> installMethods: exportedClass into: aClass [
	exportedClass methods do: [ :e | self rebuildMethod: e into: aClass ].
	exportedClass classSideMethods
		do:
			[ :e | self rebuildMethod: e into: aClass theMetaClass ]
]

{ #category : #'installing package' }
HEInstaller >> installPackage: aHEPackage [
	| newTraits newClasses |
	RPackageOrganizer default registerPackageNamed: aHEPackage packageName.

	newTraits := aHEPackage traits collect: [ :exportedTrait | self buildTrait: exportedTrait ].

	aHEPackage traits with: newTraits do: [ :exportedTrait :newTrait | self installMethods: exportedTrait into: newTrait ].

	newClasses := aHEPackage classes
		collect: [ :exportedClass | 
			| aClass |
			aClass := self build: exportedClass.
			self installClass: aClass in: exportedClass category.
			aClass ].

	aHEPackage classes with: newClasses do: [ :exportedClass :newClass | self installMethods: exportedClass into: newClass ].

	aHEPackage extensionMethods do: [ :e | self installExtensionMethod: e ].
	
	self validateNoNewUndeclared
]

{ #category : #messages }
HEInstaller >> messageExistingClass: aHEClass [
	^ 'The class ' , aHEClass className asString
		, ' already exists. Should not be overwritten'
]

{ #category : #messages }
HEInstaller >> messageExistingTrait: aHETrait [
	^ 'The trait ' , aHETrait traitName asString
		, ' already exists. Should not be overwritten'
]

{ #category : #messages }
HEInstaller >> messageMethod: aHEMethod alreadyExistsIn: aClass [
	^ 'The method ' , aHEMethod name asString , ' already exists in class ' , aClass name asString
]

{ #category : #'installing methods' }
HEInstaller >> rebuildMethod: aMethod into: aClass [
	| newMethod literalSpace extendedEnvironment |
	
	(self shouldBuildMethod: aMethod in: aClass) ifFalse: [ ^ self ].	

	extendedEnvironment := HEExtendedEnvironment new.
	extendedEnvironment inner: environment.
	extendedEnvironment newClass: aClass.
	extendedEnvironment newSelector: aMethod name.

	newMethod := CompiledMethod newMethod: aMethod bytecode size header: (aMethod headerFor: extendedEnvironment).

	extendedEnvironment newMethod: newMethod.

	aMethod literals
		doWithIndex:
			[ :literal :idx | newMethod literalAt: idx put: (literal asLiteralIn: extendedEnvironment) ].

	newMethod classBinding: aClass binding.
	literalSpace := (aMethod literals size + 1) * Smalltalk wordSize.

	aMethod bytecode doWithIndex: [ :e :idx | newMethod at: idx + literalSpace put: e ].

	aClass
		addAndClassifySelector: aMethod name
		withMethod: newMethod
		inProtocol: aMethod protocol
		notifying: nil
]

{ #category : #'reporting undeclared' }
HEInstaller >> reportNewUndeclareds: newUndeclareds [
	newUndeclareds ifNotEmpty: [ self error: 'New Undeclared created... check dependencies: ' , newUndeclareds keys printString ]
]

{ #category : #'validating existance' }
HEInstaller >> shouldBuildMethod: aHEMethod in: aClass [
	aClass
		compiledMethodAt: aHEMethod name
		ifPresent: [ :m | 
			(m isFromTrait and:[aClass isTrait not]) ifTrue:[ ^ true ].  	
			self error: (self messageMethod: aHEMethod alreadyExistsIn: aClass ) ]
		ifAbsent: [ ^ true ]
]

{ #category : #'reporting undeclared' }
HEInstaller >> validateNoNewUndeclared [
	self reportNewUndeclareds: self createdUndeclared.
	
	('[Hermes] Remaining Undeclared variables in the system: '
		, Undeclared keys printString) logCr
]
