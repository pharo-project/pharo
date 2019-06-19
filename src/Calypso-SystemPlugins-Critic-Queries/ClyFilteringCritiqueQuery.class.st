"
I am abstract critique query which subclasses wrap another critique query and filter result critiques according to their logic.

To create my instances use following expression: 

	ClyFilteringCritiqueQuery filter: aCritiqueQuery from: aScope
	
Internal Representation and Key Implementation Points.

    Instance Variables
	baseCritiqueQuery:		<ClyCritiqueQuery>
"
Class {
	#name : #ClyFilteringCritiqueQuery,
	#superclass : #ClyCritiqueQuery,
	#instVars : [
		'baseCritiqueQuery'
	],
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #'instance creation' }
ClyFilteringCritiqueQuery class >> filter: aCritiqueQuery from: aScope [

	^(self from: aScope)
		baseCritiqueQuery: aCritiqueQuery 
]

{ #category : #comparing }
ClyFilteringCritiqueQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
		
	^baseCritiqueQuery = anObject baseCritiqueQuery  
]

{ #category : #accessing }
ClyFilteringCritiqueQuery >> baseCritiqueQuery [
	^ baseCritiqueQuery
]

{ #category : #accessing }
ClyFilteringCritiqueQuery >> baseCritiqueQuery: aCritiqueQuery [
	baseCritiqueQuery := scope adoptQuery: aCritiqueQuery
]

{ #category : #execution }
ClyFilteringCritiqueQuery >> buildResult: aQueryResult [
		
	| allCritiques filtered |
	allCritiques := baseCritiqueQuery execute.
	
	filtered := allCritiques items select: [:each | self selectsCritique: each].

	aQueryResult fillWith: filtered
]

{ #category : #execution }
ClyFilteringCritiqueQuery >> checkEmptyResult [

	| allCritiques |
	allCritiques := baseCritiqueQuery execute.
	
	^(allCritiques items 
		anySatisfy: [:each | self selectsCritique: each]) not
]

{ #category : #comparing }
ClyFilteringCritiqueQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: baseCritiqueQuery hash
]

{ #category : #initialization }
ClyFilteringCritiqueQuery >> initialize [
	super initialize.
	
	baseCritiqueQuery := ClyAllBasisCritiqueQuery new
]

{ #category : #accessing }
ClyFilteringCritiqueQuery >> scope: aScope [
	super scope: aScope.
	baseCritiqueQuery scope: aScope
]

{ #category : #testing }
ClyFilteringCritiqueQuery >> selectsCritique: aCritique [
	self subclassResponsibility 
]
