"
I represent sorted query result.
I simply apply my #sortFunction for given result items.  

To create my instance use #using: method:

	ClySortedQueryResult using: ClySortByNameFunction ascending.
	
My #sortFunction can be a kind of SortFunction or ClySortFunction. 
First is valid to use in Pharo 7 because it was refactored to be safelly use in caches. SortFunction in old Pharo's do not define equality and hash. So Calypso uses its own ClySortFunction implementation in existing code.

Notice, the query result is cached as part of query requiredResult. It adds strong requirement to the values of my sortFunction variable: it should be safe for caches. This condition forbids using blocks in parameters.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	sortFunction:		<ClySortFunction>
"
Class {
	#name : #ClySortedQueryResult,
	#superclass : #ClyQueryResult,
	#instVars : [
		'sortFunction'
	],
	#category : #'Calypso-NavigationModel-Result'
}

{ #category : #'instance creation' }
ClySortedQueryResult class >> using: aSortFunction [ 
	^self new 
		sortFunction: aSortFunction 
]

{ #category : #comparing }
ClySortedQueryResult >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ sortFunction = anObject sortFunction
]

{ #category : #initialization }
ClySortedQueryResult >> defaultItems [
	^SortedCollection sortBlock: sortFunction 
]

{ #category : #building }
ClySortedQueryResult >> fillWith: objects [ 
	items addAll: objects
]

{ #category : #comparing }
ClySortedQueryResult >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: sortFunction hash
]

{ #category : #accessing }
ClySortedQueryResult >> sortFunction [
	^ sortFunction
]

{ #category : #accessing }
ClySortedQueryResult >> sortFunction: anObject [
	sortFunction := anObject
]
