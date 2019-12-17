"
I am the metaclass used for traits.
I implement all the behavior for the classSide traits.
"
Class {
	#name : #MetaclassForTraits,
	#superclass : #TraitedMetaclass,
	#instVars : [
		'users'
	],
	#category : #'TraitsV2-Base'
}

{ #category : #operations }
MetaclassForTraits >> + anotherTrait [
	"I return my self in a sequence with anotherTrait"
	^ self asTraitComposition + anotherTrait asTraitComposition
]

{ #category : #operations }
MetaclassForTraits >> - anArray [
	"I return myself with a removed method. Check TaAbstractComposition >> #- for more details"

	^ self asTraitComposition - anArray
]

{ #category : #composition }
MetaclassForTraits >> @ anArray [
	"I return myself with an aliased method. Check TaAbstractComposition >> #@ for more details"
	^ self asTraitComposition @ anArray
]

{ #category : #users }
MetaclassForTraits >> addUser: aClass [ 
	
	self users add: aClass
]

{ #category : #converting }
MetaclassForTraits >> asTraitComposition [
	^ TaClassCompositionElement for: self
]

{ #category : #'file in/out' }
MetaclassForTraits >> definition [
	^ String streamContents: [ :s | 
		s nextPutAll: self instanceSide name;
			nextPutAll: ' classTrait';
			cr; tab;
			nextPutAll: 'uses: ';
			nextPutAll: composition traitCompositionExpression.
			
		self localSlots ifNotEmpty: [ 
				s crtab;
					nextPutAll: 'slots: ';
					nextPutAll: self slotDefinitionString]]
]

{ #category : #testing }
MetaclassForTraits >> isBaseTrait [
	
	^ false
]

{ #category : #testing }
MetaclassForTraits >> isClassTrait [
	
	^ true
]

{ #category : #testing }
MetaclassForTraits >> isRejectedMethod: aSelector [
	^ (super isRejectedMethod: aSelector)
		or: [ Trait methodDict includesKey: aSelector ]
]

{ #category : #testing }
MetaclassForTraits >> isTrait [
	^ true
]

{ #category : #testing }
MetaclassForTraits >> isTraitAlias [
	^ false
]

{ #category : #testing }
MetaclassForTraits >> isTraitExclusion [
	^ false
]

{ #category : #accessing }
MetaclassForTraits >> name [

	^ thisClass ifNil: [ 'a MetaclassForTraits' ] ifNotNil: [ thisClass name asString , ' classTrait' ]
]

{ #category : #'organization updating' }
MetaclassForTraits >> notifyOfRecategorizedSelector: selector from: oldCategory to: newCategory [
	"When there is a recategorization of a selector, I propagate the changes to my users"
	super notifyOfRecategorizedSelector: selector from: oldCategory to: newCategory.
	self traitUsers do: [ :e | e recategorizeSelector: selector from: oldCategory to: newCategory ].
]

{ #category : #printing }
MetaclassForTraits >> printOn: aStream [
	aStream
		nextPutAll: self instanceSide name;
		nextPutAll: ' classTrait'
]

{ #category : #initialization }
MetaclassForTraits >> rebuildMethodDictionary [
	"I extend the behavior in TraitedMetaclass propagating the changes to my users"
	super rebuildMethodDictionary ifFalse: [ ^ false].
	self users do: [ :e | e rebuildMethodDictionary ].
	^ true
]

{ #category : #users }
MetaclassForTraits >> removeUser: aClass [
	
	self users remove: aClass ifAbsent: [  ]
]

{ #category : #accessing }
MetaclassForTraits >> traitUsers [
	^ self users
]

{ #category : #accessing }
MetaclassForTraits >> users [

	^ users ifNil: [ users := IdentitySet new ]
]

{ #category : #accessing }
MetaclassForTraits >> users: anObject [
	users := anObject
]
