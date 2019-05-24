Class {
	#name : #ClyQueryExampleReturningScopeBasis,
	#superclass : #ClyTypedQuery,
	#instVars : [
		'extraParameter'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #comparing }
ClyQueryExampleReturningScopeBasis >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ extraParameter = anObject extraParameter
]

{ #category : #execution }
ClyQueryExampleReturningScopeBasis >> buildResult: aQueryResult [ 
	"For tests we need predictable order of query result items"
	
	aQueryResult fillWith: (scope basisObjects sorted: #printString ascending)
]

{ #category : #execution }
ClyQueryExampleReturningScopeBasis >> checkEmptyResult [
	^scope basisObjects isEmpty
]

{ #category : #accessing }
ClyQueryExampleReturningScopeBasis >> extraParameter [
	^ extraParameter
]

{ #category : #accessing }
ClyQueryExampleReturningScopeBasis >> extraParameter: anObject [
	extraParameter := anObject
]

{ #category : #comparing }
ClyQueryExampleReturningScopeBasis >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: extraParameter hash
]

{ #category : #'system changes' }
ClyQueryExampleReturningScopeBasis >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	^false
]
