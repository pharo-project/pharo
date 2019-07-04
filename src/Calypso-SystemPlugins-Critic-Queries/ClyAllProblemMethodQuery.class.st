"
I return methods from my scope which include critiques from my critiquesQuery parameter.

To create my instance use following methods: 

	ClyAllProblemMethods from: aScope withCritiquesFrom: aCritiqueQuery.
	ClyAllProblemMethods from: aScope withCritiquesFromGroup: crituqueGroupName.
	
Internal Representation and Key Implementation Points.

    Instance Variables
	critiqueQuery:		<ClyCritiqueQuery>
"
Class {
	#name : #ClyAllProblemMethodQuery,
	#superclass : #ClyMethodQuery,
	#instVars : [
		'critiqueQuery'
	],
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #'instance creation' }
ClyAllProblemMethodQuery class >> from: aScope withCritiquesFrom: aCritiqueQuery [
	^(self from: aScope)
		critiqueQuery: aCritiqueQuery 
]

{ #category : #'instance creation' }
ClyAllProblemMethodQuery class >> from: aScope withCritiquesFromGroup: critiqueGroupName [
	^self 
		from: aScope 
		withCritiquesFrom: (ClyConcreteGroupCritiqueQuery 
			filter: ClyAllMethodCritiqueQuery new from: aScope byGroup: critiqueGroupName)
]

{ #category : #comparing }
ClyAllProblemMethodQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
		
	^critiqueQuery = anObject critiqueQuery  
]

{ #category : #execution }
ClyAllProblemMethodQuery >> buildResult: aQueryResult [

	| critiques methods |
	critiques := critiqueQuery execute.
	
	methods := critiques items collect: [ :each | each sourceAnchor entity ] as: IdentitySet.
	
	aQueryResult fillWith: methods
]

{ #category : #execution }
ClyAllProblemMethodQuery >> checkEmptyResult [

	^critiqueQuery hasEmptyResult
]

{ #category : #accessing }
ClyAllProblemMethodQuery >> critiqueQuery [
	^ critiqueQuery
]

{ #category : #accessing }
ClyAllProblemMethodQuery >> critiqueQuery: aCritiqueQuery [
	critiqueQuery := scope adoptQuery: aCritiqueQuery
]

{ #category : #printing }
ClyAllProblemMethodQuery >> description [

	^'problem methods (', critiqueQuery description, ')'
]

{ #category : #comparing }
ClyAllProblemMethodQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: critiqueQuery hash
]

{ #category : #initialization }
ClyAllProblemMethodQuery >> initialize [
	super initialize.
	critiqueQuery := ClyAllMethodCritiqueQuery new
]

{ #category : #'system changes' }
ClyAllProblemMethodQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	
	aSystemAnnouncement affectsCritiques ifFalse: [ ^false ].
	(super isResult: aQueryResult affectedBy: aSystemAnnouncement) ifTrue: [ ^true ].	
	"We can not compute affect on critiques by any kind system change
	because critiques are composed by some external system
	So without real knowledge we can only update critiqies after any system change.
	It can of course lead to very poor system performance because critic computation is slow process
	and triggering it after every system change can slow down system a lot.
	So instead we just reset given aQueryResult silently. So users should update views manually"
	aQueryResult forceLazyRebuild.
	^false
]

{ #category : #execution }
ClyAllProblemMethodQuery >> loadCritiquesAsync [

	^critiqueQuery async execute
]

{ #category : #printing }
ClyAllProblemMethodQuery >> printExtraInfoOn: aStream [
	super printExtraInfoOn: aStream.
	
	aStream nextPutAll: critiqueQuery description
]

{ #category : #accessing }
ClyAllProblemMethodQuery >> scope: aScope [
	super scope: aScope.
	critiqueQuery scope: aScope
]

{ #category : #testing }
ClyAllProblemMethodQuery >> selectsMethod: aMethod [
	| critiques |
	critiques := self loadCritiquesAsync.
	
	^critiques items 
		anySatisfy: [ :each | each sourceAnchor entity == aMethod ]
]
