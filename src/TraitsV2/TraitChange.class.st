"
I implement the logic to propagate the changes when a trait is modified.

I am used as:

 - A new selector has been added or updated: 

    TraitChange addSelector: #aSelector on: aTrait.

- A selector has been removed from a trait
  
   TraitChange removeSelector: #aSelector on: aTrait.

I handle the removal or the addition. 
I propagate to the users of the trait.
If the removal was shadowing a method from the trait composition, the method from the trait composition is installed and the change propagated accordly. 
"
Class {
	#name : #TraitChange,
	#superclass : #Object,
	#instVars : [
		'addedSelectors',
		'removedSelectors',
		'updatedSelectors'
	],
	#category : #'TraitsV2-Changes'
}

{ #category : #'instance creation' }
TraitChange class >> addSelector: aSelector on: aClass [

	"I propagate the addition or change of a selector in aClass that is a trait or a trait user.
	 I propagate to the users"

	^ self new
		addedSelectors: {aSelector};
		applyOn: aClass
]

{ #category : #'instance creation' }
TraitChange class >> removeSelector: aSelector on: aClass [ 

	"I propagate the removal of a selector in aClass that is a trait or a trait user. 
	If the removed method has a version in the trait composition this version is used. 
	I propagate to the users"

	^ self new
		removedSelectors: {aSelector};
		applyOn: aClass
]

{ #category : #operations }
TraitChange >> add: aSelector into: aClass changes: changes [ 

	" After adding or updating a method, it should be added to the local MethodDictionary.
	Also the change should be propagated to the users."
	aClass localMethodDict at: aSelector put: (aClass methodDict at: aSelector).
	changes updatedSelectors add: aSelector
	

]

{ #category : #accessing }
TraitChange >> addedSelectors [
	^ addedSelectors
]

{ #category : #accessing }
TraitChange >> addedSelectors: anObject [
	addedSelectors := anObject
]

{ #category : #applying }
TraitChange >> applyOn: aClass [ 	
	| result |
	
	"I apply all the changes in a class.
	This class is a trait or a trait user
	I produce a new trait change with the changes to be propagated to the users of aClass.
	Then the changes are propagated using the same mechanism recursively"
	
	result := TraitChange new.
	
	addedSelectors do: [ :e | self add: e into: aClass changes: result ].
	removedSelectors do: [ :e | self remove: e into: aClass changes: result ].
	updatedSelectors do: [ :e | self update: e into: aClass changes: result ].
	
	aClass traitUsers do: [ :aUser | result applyOn: aUser ].

]

{ #category : #initialization }
TraitChange >> initialize [
	super initialize.

	updatedSelectors := Set new.
	addedSelectors := Set new.
	removedSelectors := Set new
]

{ #category : #operations }
TraitChange >> remove: aSelector into: aClass changes: results [
	| isLocal inTrait priorMethod priorProtocol |

	"I handle the removal of a selector. There are many cases to check."

	isLocal := aClass localSelectors includes: aSelector.
	inTrait := aClass traitComposition traitDefining: aSelector ifNone: [ nil ].

	"This is only true when the update method is a propagation, so we have to avoid destroying a locally defined method.
	Check TraitedClass >> #removeSelector: and you will see that the method is removed from the local method dict 
	if it is local.
	As the method is propagated and is local, we don't do nothing"
	isLocal ifTrue: [ ^ self ].	
	
	"If the removed method was shadowing a method from a trait composition, the method from the trait composition
	should be installed. And the change propagated as an update, not as a removal."
	inTrait 
		ifNotNil: [ 
			inTrait installSelector: aSelector into: aClass.
			results updatedSelectors add: aSelector.
			^ self ].


	"We have to remove the method that was removed in a used trait."

	(aClass methodDict includesKey: aSelector) ifTrue:[
		priorMethod := aClass methodDict at: aSelector.
		priorProtocol := aClass whichCategoryIncludesSelector: aSelector.

		aClass methodDict removeKey: aSelector.
		aClass organization removeElement: aSelector.	
		
		SystemAnnouncer uniqueInstance methodRemoved: priorMethod protocol: priorProtocol origin: aClass	
	].

	"The change is propagated as a removal"
	results removedSelectors add: aSelector.
	
	"If the updated method is aliased (it has alias pointing to it), they should be removed also"
	(aClass traitComposition reverseAlias: aSelector)
		do: [ :aliased | 
			aClass methodDict removeKey: aliased ifAbsent: [  ].
			results removedSelectors add: aliased ]

]

{ #category : #accessing }
TraitChange >> removedSelectors [
	^ removedSelectors
]

{ #category : #accessing }
TraitChange >> removedSelectors: anObject [
	removedSelectors := anObject
]

{ #category : #operations }
TraitChange >> update: aSelector into: aClass changes: changes [
	
	"I am the responsible of propagating a change from an used trait"
	
	"If the updated selector (from other trait) is in the local methods, nothing to do."

	aClass localMethodDict at: aSelector ifPresent: [ ^ self ].
	
	(aClass traitComposition selectors includes: aSelector)
		ifTrue: [  	aClass traitComposition installSelector: aSelector into: aClass.
						changes updatedSelectors add: aSelector ].

	"If the updated method is aliased, it should be updated also"
	(aClass traitComposition reverseAlias: aSelector)
		do: [ :aliased | 
			aClass traitComposition installSelector: aliased into: aClass.
			changes updatedSelectors add: aliased ]
]

{ #category : #accessing }
TraitChange >> updatedSelectors [
	^ updatedSelectors
]

{ #category : #accessing }
TraitChange >> updatedSelectors: anObject [
	updatedSelectors := anObject
]
