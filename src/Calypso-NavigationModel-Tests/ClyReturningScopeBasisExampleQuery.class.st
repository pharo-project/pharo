Class {
	#name : #ClyReturningScopeBasisExampleQuery,
	#superclass : #ClyTypedQuery,
	#instVars : [
		'extraParameter'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #comparing }
ClyReturningScopeBasisExampleQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ extraParameter = anObject extraParameter
]

{ #category : #execution }
ClyReturningScopeBasisExampleQuery >> buildResult: aQueryResult [ 
	"For tests we need predictable order of query result items"
	
	aQueryResult fillWith: (scope basisObjects sorted: #printString ascending)
]

{ #category : #execution }
ClyReturningScopeBasisExampleQuery >> checkEmptyResult [
	^scope basisObjects isEmpty
]

{ #category : #accessing }
ClyReturningScopeBasisExampleQuery >> extraParameter [
	^ extraParameter
]

{ #category : #accessing }
ClyReturningScopeBasisExampleQuery >> extraParameter: anObject [
	extraParameter := anObject
]

{ #category : #comparing }
ClyReturningScopeBasisExampleQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: extraParameter hash
]

{ #category : #'system changes' }
ClyReturningScopeBasisExampleQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	^false
]
