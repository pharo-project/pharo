"
I am the entry point to load a package of Hermes.

I am the basic implementation, I can only handle basic cases:
- If there is a new undeclared I produce an error.
- If there is an existing element I produce an error.

For more extended options check HEExtendedEnvironment

Example: 

	file := 'aFile.hermes'.
	installer := HEInstaller new.
	
	reader := HEBinaryReader new
		stream: (File named: file) readStream;
		yourself.

	readPackage := HEPackage readFrom: reader.

	installer installPackage: readPackage
"
Class {
	#name : 'HEInstaller',
	#superclass : 'Object',
	#instVars : [
		'environment',
		'originalUndeclareds',
		'hasTraits'
	],
	#category : 'Hermes-ClassInstaller',
	#package : 'Hermes',
	#tag : 'ClassInstaller'
}

{ #category : 'creating classes' }
HEInstaller >> build: aHEClass [

	| newClass superclass layoutType |
	(self existingClass: aHEClass) ifNotNil: [ :x | ^ x ].

	superclass := self classNamed: aHEClass superclass.
	layoutType := self classNamed: aHEClass layoutClass.

	newClass := self class classInstaller make: [ :builder |
		            builder
			            name: aHEClass className;
			            superclass: superclass;
			            layoutClass: layoutType;
			            slots: aHEClass instanceVariables asSlotCollection;
			            sharedVariablesFromString: aHEClass classVariables;
			            sharedPools: aHEClass sharedPools;
			            package: aHEClass packageName;
			            classSlots: aHEClass classInstancevariables asSlotCollection.
		            aHEClass tagName ifNotEmpty: [ :tag | builder tag: tag ].
		            self supportsTraits ifTrue: [
			            builder
				            traitComposition: (self buildTraitCompositionFor: aHEClass traitComposition);
				            classTraitComposition: (self buildTraitCompositionFor: aHEClass classTraitComposition) ] ].
	^ newClass
]

{ #category : 'creating traits' }
HEInstaller >> buildTrait: aTraitDefinition [

	(self existingTrait: aTraitDefinition) ifNotNil: [ :x | ^ x ].

	self supportsTraits ifFalse: [ self error: 'Trait support is not installed' ].

	^ self class classInstaller make: [ :builder |
		  builder
			  beTrait;
			  name: aTraitDefinition traitName;
			  traitComposition: (self buildTraitCompositionFor: aTraitDefinition traitComposition);
			  classTraitComposition: (self buildTraitCompositionFor: aTraitDefinition classTraitComposition);
			  package: aTraitDefinition packageName;
			  environment: environment.

		  aTraitDefinition tagName ifNotEmpty: [ :tag | builder tag: tag ] ]
]

{ #category : 'creating traits' }
HEInstaller >> buildTraitCompositionFor: traitComposition [

	^ (traitComposition asLiteralIn: environment) asTraitComposition
]

{ #category : 'accessing' }
HEInstaller >> classNamed: aSymbol [
	aSymbol ifNil: [ ^ nil ].
	^ self environment at: aSymbol
]

{ #category : 'reporting undeclared' }
HEInstaller >> createdUndeclared [
	^ (Undeclared associations reject: [ :asoc | originalUndeclareds associations includes: asoc]) asDictionary 
]

{ #category : 'installing package' }
HEInstaller >> doInstallPackage: aHEPackage [

	| newTraits newClasses |
	"Creating the package. It requires a number of steps.
	1. Register the package in the organizer."
	self flag: #pahro11. "Update this when Pharo 12 will be the minimal version"
	SystemVersion current major < 12 ifTrue: [ RPackageOrganizer default registerPackageNamed: aHEPackage packageName ] ifFalse: [ self packageOrganizer ensurePackage: aHEPackage packageName ].

	"2. Install the traits"
	newTraits := aHEPackage traits collect: [ :exportedTrait | self buildTrait: exportedTrait ].

	"3. Install traits methods"
	aHEPackage traits with: newTraits do: [ :exportedTrait :newTrait | self installMethods: exportedTrait into: newTrait ].

	"4. Install classes"
	newClasses := aHEPackage classes collect: [ :exportedClass | self build: exportedClass ].

	"5. Install class methods"
	aHEPackage classes with: newClasses do: [ :exportedClass :newClass | self installMethods: exportedClass into: newClass ].

	"6. Install extension methods"
	aHEPackage extensionMethods do: [ :e | self installExtensionMethod: e ].

	"7. After all I validate if there are no new undeclared variables created in the environment.
	A new undeclared is a sign of an improper modularization."
	self validateNoNewUndeclared
]

{ #category : 'accessing' }
HEInstaller >> environment [
	^ environment
]

{ #category : 'accessing' }
HEInstaller >> environment: anObject [
	environment := anObject
]

{ #category : 'validating existence' }
HEInstaller >> existingClass: aHEClass [
	(environment hasClassNamed: aHEClass className) ifTrue:[
		self error: (self messageExistingClass: aHEClass)
	].
	^ nil
]

{ #category : 'validating existence' }
HEInstaller >> existingTrait: aHETrait [
	(environment includesKey: aHETrait traitName) ifTrue:[ 
		self error: (self messageExistingTrait: aHETrait)
	].
	
	^nil
]

{ #category : 'initialization' }
HEInstaller >> initialize [

	environment := self class environment.
	originalUndeclareds := Undeclared copy.
	"We need to set it at the initialization and we cannot ask this later during the building because it will cause trouble if the class we are building is Trait."
	hasTraits := Smalltalk globals hasClassNamed: #Trait
]

{ #category : 'installing methods' }
HEInstaller >> installExtensionMethod: extensionMethod [
	| aClass |
	aClass := self environment classNamed: extensionMethod className.
	
	aClass ifNil: [ self error: 'Required class named: ' , extensionMethod className, ' does not exists.' ].
	
	self rebuildMethod: extensionMethod into: aClass.
]

{ #category : 'installing methods' }
HEInstaller >> installMethods: exportedClass into: aClass [
	exportedClass methods do: [ :e | self rebuildMethod: e into: aClass ].
	exportedClass classSideMethods
		do:
			[ :e | self rebuildMethod: e into: aClass classSide ]
]

{ #category : 'installing package' }
HEInstaller >> installPackage: aHEPackage [
	| storedAnnouncements |
	
	storedAnnouncements := SystemAnnouncer uniqueInstance suspendAllWhileStoring: [ self doInstallPackage: aHEPackage ].	
	storedAnnouncements do: [ :e | SystemAnnouncer uniqueInstance announce: e ].
]

{ #category : 'messages' }
HEInstaller >> messageExistingClass: aHEClass [
	^ 'The class ' , aHEClass className asString
		, ' already exists. Should not be overwritten'
]

{ #category : 'messages' }
HEInstaller >> messageExistingTrait: aHETrait [
	^ 'The trait ' , aHETrait traitName asString
		, ' already exists. Should not be overwritten'
]

{ #category : 'messages' }
HEInstaller >> messageMethod: aHEMethod alreadyExistsIn: aClass [
	^ 'The method ' , aHEMethod name asString , ' already exists in class ' , aClass name asString
]

{ #category : 'installing methods' }
HEInstaller >> rebuildMethod: aMethod into: aClass [

	| newMethod literalSpace extendedEnvironment |
	(self shouldBuildMethod: aMethod in: aClass) ifFalse: [ ^ self ].

	extendedEnvironment := HEExtendedEnvironment new.
	extendedEnvironment inner: environment.
	extendedEnvironment newClass: aClass.
	extendedEnvironment newSelector: aMethod name.

	self flag: #pharo11. "If Pharo12 the management of the method trailer changed and the way to create a new method changed too."
	newMethod := CompiledMethod
		             newMethod: (SystemVersion current major >= 12
				              ifTrue: [ aMethod bytecode size + CompiledMethod trailerSize ]
				              ifFalse: [ aMethod bytecode size ])
		             header: (aMethod headerFor: extendedEnvironment).

	extendedEnvironment newMethod: newMethod.

	aMethod literals doWithIndex: [ :literal :idx | newMethod literalAt: idx put: (literal asLiteralIn: extendedEnvironment) ].

	newMethod classBinding: aClass binding.
	literalSpace := aMethod literals size + 1 * Smalltalk wordSize.

	aMethod bytecode doWithIndex: [ :e :idx | newMethod at: idx + literalSpace put: e ].

	aClass addAndClassifySelector: aMethod name withMethod: newMethod inProtocol: aMethod protocol
]

{ #category : 'reporting undeclared' }
HEInstaller >> reportNewUndeclareds: newUndeclareds [
	newUndeclareds ifNotEmpty: [ self error: 'New Undeclared created... check dependencies: ' , newUndeclareds keys printString ]
]

{ #category : 'validating existence' }
HEInstaller >> shouldBuildMethod: aHEMethod in: aClass [
	aClass
		compiledMethodAt: aHEMethod name
		ifPresent: [ :m | 
			(m isFromTrait and:[aClass isTrait not]) ifTrue:[ ^ true ].  	
			self error: (self messageMethod: aHEMethod alreadyExistsIn: aClass ) ]
		ifAbsent: [ ^ true ]
]

{ #category : 'testing' }
HEInstaller >> supportsTraits [

	^ hasTraits
]

{ #category : 'reporting undeclared' }
HEInstaller >> validateNoNewUndeclared [
	self reportNewUndeclareds: self createdUndeclared.
	
	SystemNotification signal: ('[Hermes] Remaining Undeclared variables in the system: '
		, Undeclared keys printString)
]
