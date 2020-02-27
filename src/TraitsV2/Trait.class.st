"
My instances are the traits used to create traited classes.
The traits can be combined in different ways check TaAbstractComposition to see the different possible operations.

I am a subclass of Class, and I implement the specific behavior for traits.
So, the traits are polymorphic with classes.

I support stateful traits. 
Also Traits uses other metaclass to implement the behavior, check MetaclassForTraits. 

Also a nice diagram can be seen evaluating:

((ZnEasy getPng: 'https://raw.githubusercontent.com/wiki/pharo-project/pharo/figures/traits.png') 
	asMorph resize: 800@800; openInWindow) 


"
Class {
	#name : #Trait,
	#superclass : #Class,
	#instVars : [
		'users'
	],
	#classInstVars : [
		'users'
	],
	#category : #'TraitsV2-Base'
}

{ #category : #private }
Trait class >> configureBuilder: builder withName: aName traitComposition: aTraitCompositionOrCollection slots: someSlots packageName: aPackageName [

	^ builder
		name: aName;
		superclass: nil;
		metaSuperclass: self;
		metaclassClass: MetaclassForTraits;
		slots: someSlots;
		sharedVariables: '';
		sharedPools: '';
		category: aPackageName;
		traitComposition: aTraitCompositionOrCollection asTraitComposition;
		classTraitComposition: aTraitCompositionOrCollection asTraitComposition classComposition;
		classSlots: #()
]

{ #category : #'instance creation' }
Trait class >> named: aSymbol [
	^ self named: aSymbol uses: {} package: 'Unclassified'
]

{ #category : #'instance creation' }
Trait class >> named: aSymbol uses: aCompositionOrArray [
	^ self named: aSymbol uses: aCompositionOrArray package: 'Unclassified'
]

{ #category : #deprecated }
Trait class >> named: aSymbol uses: aTraitCompositionOrCollection category: aString [
	^ self
		named: aSymbol
		uses: aTraitCompositionOrCollection
		package: aString
		env: self environment
]

{ #category : #deprecated }
Trait class >> named: aName uses: aTraitCompositionOrCollection category: aString env: anEnvironment [
	^ self
		named: aName
		uses: aTraitCompositionOrCollection
		slots: #()
		package: aString
		env: anEnvironment
]

{ #category : #'instance creation' }
Trait class >> named: aSymbol uses: aTraitCompositionOrCollection package: aString [
	^ self
		named: aSymbol
		uses: aTraitCompositionOrCollection
		package: aString
		env: self environment
]

{ #category : #'instance creation' }
Trait class >> named: aName uses: aTraitCompositionOrCollection package: aString env: anEnvironment [
	^ self
		named: aName
		uses: aTraitCompositionOrCollection
		slots: #()
		package: aString
		env: anEnvironment
]

{ #category : #deprecated }
Trait class >> named: aName uses: aTraitCompositionOrCollection slots: someSlots category: aCategory [

	^ self named: aName 
		uses: aTraitCompositionOrCollection 
		slots: someSlots 
		package: aCategory 
		env: self environment 
]

{ #category : #deprecated }
Trait class >> named: aName uses: aTraitCompositionOrCollection slots: someSlots category: aCategory env: anEnvironment [ 
	"It is unclear that it is not use by low level library such as MC so we should not use it but we do not remove it for now."
	
	^ self named: aName uses: aTraitCompositionOrCollection slots: someSlots package: aCategory env: anEnvironment 
]

{ #category : #'instance creation' }
Trait class >> named: aName uses: aTraitCompositionOrCollection slots: someSlots package: aPackageName [

	^ self named: aName 
		uses: aTraitCompositionOrCollection 
		slots: someSlots 
		package: aPackageName 
		env: self environment 
]

{ #category : #'instance creation' }
Trait class >> named: aName uses: aTraitCompositionOrCollection slots: someSlots package: aPackageName env: anEnvironment [
	| trait |
	trait := self classInstaller
		make: [ :builder | 
			self
				configureBuilder: builder
				withName: aName
				traitComposition: aTraitCompositionOrCollection
				slots: someSlots
				packageName: aPackageName ].
	self assert: [ trait class class = MetaclassForTraits ].
	self assert: [ trait class superclass = Trait ].
	^ trait
]

{ #category : #accessing }
Trait >> + anotherTrait [
	"I return my self in a sequence with anotherTrait"
	^ self asTraitComposition + anotherTrait asTraitComposition
]

{ #category : #accessing }
Trait >> - anArray [
	"I return myself with a removed method. Check TaAbstractComposition >> #- for more details"

	^ self asTraitComposition - anArray
]

{ #category : #accessing }
Trait >> -- aSlotName [

	^ self asTraitComposition -- aSlotName
]

{ #category : #accessing }
Trait >> @ anArray [
	"I return myself with an aliased method. Check TaAbstractComposition >> #@ for more details"
	^ self asTraitComposition @ anArray
]

{ #category : #users }
Trait >> addUser: aClass [ 

	self users add: aClass
]

{ #category : #accessing }
Trait >> asTraitComposition [
	^ TaCompositionElement for: self.
]

{ #category : #'instance creation' }
Trait >> basicNew [
	"Traits should never be instantiated.
	They are naked object: superclass = nil. 
	They not understand any Object messages 
	and any DNU crashes VM because there are no methods doesNotUnderstand: or cannotInterpret:"
	
	self error: 'Traits should not be instantiated!'
]

{ #category : #'accessing parallel hierarchy' }
Trait >> classTrait [

	^ self class
]

{ #category : #'file in/out' }
Trait >> definition [
	
	(self instanceSide name == #Trait) ifTrue: [ ^ super definition ].

	^ String streamContents: [ :s | 
		s 
			nextPutAll: 'Trait named: ';
			nextPutAll:	self name printString;
			cr; tab; 
			nextPutAll: ' uses: ';
			nextPutAll: self traitComposition traitCompositionExpression; 
			cr.
		self classLayout visibleSlots ifNotEmpty: [ 
			s tab; nextPutAll: ' slots: ';
			nextPutAll: self slotDefinitionString; cr. ].
		s tab; nextPutAll: ' package: ';
			nextPutAll: self category asString printString
	]
]

{ #category : #testing }
Trait >> isBaseTrait [
	
	^ true
]

{ #category : #testing }
Trait >> isClass [
	
	^ false
]

{ #category : #testing }
Trait >> isClassTrait [
	
	^ false
]

{ #category : #testing }
Trait >> isTrait [
	^ true
]

{ #category : #testing }
Trait >> isTraitAlias [
	^ false
]

{ #category : #testing }
Trait >> isTraitExclusion [
	^ false
]

{ #category : #'organization updating' }
Trait >> notifyOfRecategorizedSelector: selector from: oldCategory to: newCategory [
	"When there is a recategorization of a selector, I propagate the changes to my users"
	super notifyOfRecategorizedSelector: selector from: oldCategory to: newCategory.
	self traitUsers do: [ :e | e recategorizeSelector: selector from: oldCategory to: newCategory ].
]

{ #category : #'file in/out' }
Trait >> oldDefinition [

	^ self definition
]

{ #category : #'accessing method dictionary' }
Trait >> rebuildMethodDictionary [
	"I extend the behavior in TraitedMetaclass propagating the changes to my users"
	self doRebuildMethodDictionary ifFalse: [ ^ false ].
	self users do: [ :e | e rebuildMethodDictionary ].
	^ true
]

{ #category : #initialization }
Trait >> removeFromSystem: logged [
	
	"When a trait is removed from the system it should:
	
	- Remove it self from its trait compositions.
	- Remove it self from its users.
	- Remove its classTrait from its users.
	- Do what any class does."
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
