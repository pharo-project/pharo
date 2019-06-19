"
I represent query which filters results of another query.
My instances can be created by:

	ClyFilterQuery for: anQuery filter: anItemFilter

Or you can simply convert any query using filter: 

	aQuery filteredBy: anItemFilter

During execution I evaluate #actualQuery and filter received items with my #itemFilter.
Look at method #buildResult: for details.

itemFilter is a kind of ClyItemFilter. For example there is ClyItemNameFilter which matches items by name using specified pattern. Look ClyItemFilter comments for details. 

Internal Representation and Key Implementation Points.

    Instance Variables
	itemFilter:		<ClyItemFilter>
"
Class {
	#name : #ClyFilterQuery,
	#superclass : #ClyWrapperQuery,
	#instVars : [
		'itemFilter'
	],
	#category : #'Calypso-NavigationModel-Query'
}

{ #category : #'instance creation' }
ClyFilterQuery class >> for: originalQuery filter: aClyItemFilter [
	^(self with: {originalQuery})
		itemFilter: aClyItemFilter 
]

{ #category : #comparing }
ClyFilterQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ itemFilter = anObject itemFilter
]

{ #category : #execution }
ClyFilterQuery >> buildResult: aQueryResult [
	
	| fullResult filteredItems |
	fullResult := self actualQuery execute.
	filteredItems := fullResult items 
		select: [ :each | itemFilter matches: each ] 
		thenCollect: [ :each | each adoptForNewCalypsoQuery].
	aQueryResult items: filteredItems
]

{ #category : #printing }
ClyFilterQuery >> description [
	^super description, ' filtered by ', itemFilter printCriteria
]

{ #category : #comparing }
ClyFilterQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: itemFilter hash
]

{ #category : #accessing }
ClyFilterQuery >> itemFilter [
	^ itemFilter
]

{ #category : #accessing }
ClyFilterQuery >> itemFilter: anObject [
	itemFilter := anObject
]
