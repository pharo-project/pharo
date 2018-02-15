Class {
	#name : #Trait,
	#superclass : #Class,
	#instVars : [
		'users'
	],
	#classInstVars : [
		'users'
	],
	#category : #TraitsV2
}

{ #category : #'instance creation' }
Trait class >> named: aSymbol [
	^ self named: aSymbol uses: {} category: 'Unclassified'
]

{ #category : #'instance creation' }
Trait class >> named: aSymbol uses: aCompositionOrArray [
	^ self named: aSymbol uses: aCompositionOrArray category: 'Unclassified'
]

{ #category : #'instance creation' }
Trait class >> named: aSymbol uses: aTraitCompositionOrCollection category: aString [
	^ self
		named: aSymbol
		uses: aTraitCompositionOrCollection
		category: aString
		env: self environment
]

{ #category : #'instance creation' }
Trait class >> named: aName uses: aTraitCompositionOrCollection category: aString env: anEnvironment [
	^ self
		named: aName
		uses: aTraitCompositionOrCollection
		slots: #()
		category: aString
		env: anEnvironment
]

{ #category : #'instance creation' }
Trait class >> named: aSymbol uses: aTraitCompositionOrCollection package: aString [
	^ self
		named: aSymbol
		uses: aTraitCompositionOrCollection
		category: aString
		env: self environment
]

{ #category : #'instance creation' }
Trait class >> named: aName uses: aTraitCompositionOrCollection slots: someSlots category: aCategory env: anEnvironment [ 

	| trait |

	trait:= self classInstaller
		make: [ :builder | 
			builder
				name: aName;
				superclass: nil;
				metaSuperclass: self;
				slots: someSlots;
				sharedVariables: '';
				sharedPools: '';
				category: aCategory;
				traitComposition: aTraitCompositionOrCollection asTraitComposition;
				classTraitComposition: aTraitCompositionOrCollection asTraitComposition classComposition;
				classSlots: #() ].
			
	self assert: [ trait class class = MetaclassForTraits ].
	self assert: [ trait class superclass = Trait ].
	
	^ trait
]

{ #category : #accessing }
Trait >> + anotherTrait [
	^ self asTraitComposition + anotherTrait asTraitComposition
]

{ #category : #accessing }
Trait >> - anArray [
	^ self asTraitComposition - anArray
]

{ #category : #accessing }
Trait >> @ anArray [
	^ self asTraitComposition @ anArray
]

{ #category : #users }
Trait >> addUser: aClass [ 

	self users add: aClass
]

{ #category : #'class variables' }
Trait >> allClassVarNames [
	
	^ #()
]

{ #category : #accessing }
Trait >> asTraitComposition [
	^ TaCompositionElement for: self.
]

{ #category : #'accessing parallel hierarchy' }
Trait >> classTrait [

	^ self class
]

{ #category : #'filein/out' }
Trait >> definition [
	
	(self baseClass name == #Trait) ifTrue: [ ^ super definition ].

	^ String streamContents: [ :s | 
		s 
			nextPutAll: 'Trait named: ';
			nextPutAll:	self name printString;
			cr; tab;
			nextPutAll: ' uses: ';
			nextPutAll: self traitComposition traitCompositionExpression;
			cr; tab;
			nextPutAll: ' category: ';
			nextPutAll: self category asString printString
	]
]

{ #category : #accessing }
Trait >> definitionForNautilus [

	^ self definition
]

{ #category : #accessing }
Trait >> definitionForNautilusWithSlots [

	^ self definition
]

{ #category : #testing }
Trait >> isBaseTrait [
	
	^ true
]

{ #category : #testing }
Trait >> isClassTrait [
	
	^ false
]

{ #category : #testing }
Trait >> isTrait [
	^ true
]

{ #category : #'organization updating' }
Trait >> notifyOfRecategorizedSelector: selector from: oldCategory to: newCategory [
	super notifyOfRecategorizedSelector: selector from: oldCategory to: newCategory.
	self traitUsers do: [ :e | e recategorizeSelector: selector from: oldCategory to: newCategory ].
]

{ #category : #'accessing method dictionary' }
Trait >> rebuildMethodDictionary [
	self doRebuildMethodDictionary ifFalse: [ ^ false ].
	self users do: [ :e | e rebuildMethodDictionary ].
	^ true
]

{ #category : #'initialize-release' }
Trait >> removeFromSystem: logged [
	
	self traitComposition removeUser: self.
	self class traitComposition removeUser: self class.
	
	self traitUsers do: [ :e | e isObsolete ifFalse: [ e removeFromComposition: self ]].
	self class traitUsers do: [ :e | e isObsolete ifFalse: [e removeFromComposition: self class ]].
	^ super removeFromSystem: logged
]

{ #category : #removing }
Trait >> removeUser: aClass [ 

	self users remove: aClass ifAbsent: [ ].
	
	
]

{ #category : #accessing }
Trait >> traitUsers [
	^ self users
]

{ #category : #users }
Trait >> users [
	^ users ifNil: [ users := IdentitySet new ]
]

{ #category : #accessing }
Trait >> users: anObject [
	users := anObject
]
