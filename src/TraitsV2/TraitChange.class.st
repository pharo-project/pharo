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

	^ self new
		addedSelectors: {aSelector};
		applyOn: aClass
]

{ #category : #'instance creation' }
TraitChange class >> removeSelector: aSelector on: aClass [ 
	^ self new
		removedSelectors: {aSelector};
		applyOn: aClass
]

{ #category : #operations }
TraitChange >> add: aSelector into: aClass changes: changes [ 

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
	| isLocal inTrait priorMethod selector priorProtocol |
	isLocal := aClass localSelectors includes: aSelector.
	inTrait := aClass traitComposition traitDefining: aSelector ifNone: [ nil ].

	"This is only true when the method is propagated, so we have to avoid destroying a locally defined method"
	isLocal ifTrue: [ ^ self ].	
	
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

	results removedSelectors add: aSelector.
	
	"If the updated method is aliased, it should be removed also"
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
