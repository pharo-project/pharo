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
	#name : 'Trait',
	#superclass : 'Class',
	#instVars : [
		'users'
	],
	#category : 'TraitsV2-Base',
	#package : 'TraitsV2',
	#tag : 'Base'
}

{ #category : 'old pharo 8 syntax' }
Trait class >> named: aName [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  beTrait ]
]

{ #category : 'old pharo 8 syntax' }
Trait class >> named: aName instanceVariableNames: aString package: aPackageName [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  slotsFromString: aString;
			  category: aPackageName;
			  beTrait ]
]

{ #category : 'old pharo 8 syntax' }
Trait class >> named: aName package: aString [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  category: aString;
			  beTrait ]
]

{ #category : 'old pharo 8 syntax' }
Trait class >> named: aName uses: aCompositionOrArray [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  traitComposition: aCompositionOrArray;
			  beTrait ]
]

{ #category : 'deprecated' }
Trait class >> named: aName uses: aTraitCompositionOrCollection category: aPackageName [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  category: aPackageName;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : 'deprecated' }
Trait class >> named: aName uses: aTraitCompositionOrCollection category: aPackageName env: anEnvironment [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  category: aPackageName;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : 'old pharo 8 syntax' }
Trait class >> named: aName uses: aTraitCompositionOrCollection instanceVariableNames: aString package: aPackageName [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  slotsFromString: aString;
			  category: aPackageName;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : 'old pharo 8 syntax' }
Trait class >> named: aName uses: aTraitCompositionOrCollection instanceVariableNames: aString package: aPackageName env: anEnvironment [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  slotsFromString: aString;
			  category: aPackageName;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : 'old pharo 8 syntax' }
Trait class >> named: aName uses: aTraitCompositionOrCollection package: aString [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  category: aString;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : 'old pharo 8 syntax' }
Trait class >> named: aName uses: aTraitCompositionOrCollection package: aString env: anEnvironment [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  category: aString;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : 'deprecated' }
Trait class >> named: aName uses: aTraitCompositionOrCollection slots: someSlots category: aCategory [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  slots: someSlots;
			  category: aCategory;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : 'deprecated' }
Trait class >> named: aName uses: aTraitCompositionOrCollection slots: someSlots category: aCategory env: anEnvironment [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  slots: someSlots;
			  category: aCategory;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : 'old pharo 8 syntax' }
Trait class >> named: aName uses: aTraitCompositionOrCollection slots: someSlots package: aPackageName [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  slots: someSlots;
			  category: aPackageName;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : 'old pharo 8 syntax' }
Trait class >> named: aName uses: aTraitCompositionOrCollection slots: someSlots package: aPackageName env: anEnvironment [

	^ self classInstaller make: [ :builder |
		  builder
			  name: aName;
			  slots: someSlots;
			  category: aPackageName;
			  traitComposition: aTraitCompositionOrCollection;
			  beTrait ]
]

{ #category : 'accessing' }
Trait >> + anotherTrait [
	"I return my self in a sequence with anotherTrait"
	^ self asTraitComposition + anotherTrait asTraitComposition
]

{ #category : 'accessing' }
Trait >> - anArray [
	"I return myself with a removed method. Check TaAbstractComposition >> #- for more details"

	^ self asTraitComposition - anArray
]

{ #category : 'accessing' }
Trait >> -- aSlotNameCollection [

	^ self asTraitComposition -- aSlotNameCollection
]

{ #category : 'accessing' }
Trait >> @ anArray [
	"I return myself with an aliased method. Check TaAbstractComposition >> #@ for more details"
	^ self asTraitComposition @ anArray
]

{ #category : 'accessing' }
Trait >> @@ anArray [
	"I return myself with an aliased method. Check TaAbstractComposition >> #@ for more details"
	^ self asTraitComposition @@ anArray
]

{ #category : 'users' }
Trait >> addUser: aClass [

	self users add: aClass
]

{ #category : 'instance creation' }
Trait >> basicNew [
	"Traits should never be instantiated.
	They are naked object: superclass = nil.
	They not understand any Object messages
	and any DNU crashes VM because there are no methods doesNotUnderstand: or cannotInterpret:"

	self error: 'Traits should not be instantiated!'
]

{ #category : 'accessing - parallel hierarchy' }
Trait >> classTrait [

	^ self class
]

{ #category : 'fileout' }
Trait >> definitionStringFor: aConfiguredPrinter [

	^ aConfiguredPrinter traitDefinitionString
]

{ #category : 'fileout' }
Trait >> expandedDefinitionStringFor: aPrinter [

	^ aPrinter expandedTraitDefinitionString
]

{ #category : 'testing' }
Trait >> isBaseTrait [

	^ true
]

{ #category : 'testing' }
Trait >> isClass [

	^ false
]

{ #category : 'testing' }
Trait >> isClassTrait [

	^ false
]

{ #category : 'testing' }
Trait >> isEmpty [
	"Since we can have potential an empty array in the fluid definition (to be revisited)
	when the trait composition is aTrait it received the message isEmpty and would break
	traitComposition
		ifNotNil: [ :t | t isEmpty ifFalse: [shiftClassBuilder traitComposition: traitComposition ]]."

	^ false
]

{ #category : 'testing' }
Trait >> isTrait [
	^ true
]

{ #category : 'testing' }
Trait >> isTraitAlias [
	^ false
]

{ #category : 'testing' }
Trait >> isTraitExclusion [
	^ false
]

{ #category : 'testing' }
Trait >> isUsed [

	^ super isUsed or: [ self traitUsers notEmpty ]
]

{ #category : 'organization updating' }
Trait >> notifyOfRecategorizedSelector: selector from: oldProtocol to: newProtocol [
	"When there is a recategorization of a selector, I propagate the changes to my users"

	super notifyOfRecategorizedSelector: selector from: oldProtocol to: newProtocol.
	self traitUsers do: [ :e | e recategorizeSelector: selector from: oldProtocol to: newProtocol ]
]

{ #category : 'accessing - method dictionary' }
Trait >> rebuildMethodDictionary [
	"I extend the behavior in TraitedMetaclass propagating the changes to my users"
	self doRebuildMethodDictionary ifFalse: [ ^ false ].
	self users do: [ :e | e rebuildMethodDictionary ].
	^ true
]

{ #category : 'initialization' }
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

{ #category : 'removing' }
Trait >> removeUser: aClass [

	self users remove: aClass ifAbsent: [ ]
]

{ #category : 'accessing' }
Trait >> traitUsers [
	^ self users
]

{ #category : 'users' }
Trait >> users [
	^ users ifNil: [ users := IdentitySet new ]
]

{ #category : 'accessing' }
Trait >> users: anObject [
	users := anObject
]
