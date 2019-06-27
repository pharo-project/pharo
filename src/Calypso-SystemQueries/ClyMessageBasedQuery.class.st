"
I am a root of hierarchy of method queries which uses set of selectors to filter methods.

My instances can be created using following methods:

	ClyMessageSenders of: #selector.
	ClyMessageImplementors of: #selector from: aScope.
	ClyMessageSenders ofAny: #(selector1 selector2).
	ClyMessageImplementors ofAny: #(selector1 selector2) from: aScope.

I provide suitable printing methods and correct comparison implementation.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	selectors:		<Array of<Symbol>>
"
Class {
	#name : #ClyMessageBasedQuery,
	#superclass : #ClyMethodQuery,
	#instVars : [
		'selectors'
	],
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'instance creation' }
ClyMessageBasedQuery class >> of: aSelector [
	^self ofAny: { aSelector }
]

{ #category : #'instance creation' }
ClyMessageBasedQuery class >> of: aSelector from: aScope [
	^self ofAny: {aSelector} from: aScope
]

{ #category : #'instance creation' }
ClyMessageBasedQuery class >> ofAny: selectors [
	^self new 
		selectors: selectors
]

{ #category : #'instance creation' }
ClyMessageBasedQuery class >> ofAny: selectors from: aScope [
	^aScope adoptQuery: (self ofAny: selectors)
]

{ #category : #comparing }
ClyMessageBasedQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^selectors = anObject selectors
]

{ #category : #comparing }
ClyMessageBasedQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ super hash bitXor: selectors hash
]

{ #category : #printing }
ClyMessageBasedQuery >> printExtraInfoOn: aStream [
	super printExtraInfoOn: aStream.

	aStream nextPutAll: self printSelectors
]

{ #category : #printing }
ClyMessageBasedQuery >> printSelectors [

	^',' join: selectors
]

{ #category : #accessing }
ClyMessageBasedQuery >> selectors [
	^ selectors
]

{ #category : #accessing }
ClyMessageBasedQuery >> selectors: anObject [
	selectors := anObject
]
