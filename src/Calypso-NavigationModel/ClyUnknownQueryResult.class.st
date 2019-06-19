"
I implement null pattern for query result.

Users should use my singleton #instance on class side: 

	ClyUnknownQueryResult instance
"
Class {
	#name : #ClyUnknownQueryResult,
	#superclass : #ClyQueryResult,
	#classInstVars : [
		'instance'
	],
	#category : #'Calypso-NavigationModel-Result'
}

{ #category : #singleton }
ClyUnknownQueryResult class >> instance [
	^instance ifNil: [instance := ClyUnknownQueryResult new]
]

{ #category : #accessing }
ClyUnknownQueryResult >> adoptForBrowser [
	^self
]

{ #category : #private }
ClyUnknownQueryResult >> beProtected [

]

{ #category : #initialization }
ClyUnknownQueryResult >> collectMetadata [
	metadata := ClyQueryResultMetadata new
]

{ #category : #initialization }
ClyUnknownQueryResult >> defaultItems [
	^#()
]

{ #category : #building }
ClyUnknownQueryResult >> fillWith: objects [
]

{ #category : #queries }
ClyUnknownQueryResult >> findItemsSimilarTo: sampleEnvironmentItems [
	^#()
]

{ #category : #queries }
ClyUnknownQueryResult >> findItemsWith: aCollection [ 
	^Array new: aCollection size withAll: nil
]

{ #category : #building }
ClyUnknownQueryResult >> forceLazyRebuild [
]

{ #category : #building }
ClyUnknownQueryResult >> forceRebuild [
]

{ #category : #initialization }
ClyUnknownQueryResult >> initialize [
	super initialize.
	needsRebuild := false.
	self initializeItems.
	self collectMetadata
]

{ #category : #'system changes' }
ClyUnknownQueryResult >> itemsChanged [
]

{ #category : #queries }
ClyUnknownQueryResult >> itemsStartingAt: startIndex count: size [
	^#()
]

{ #category : #private }
ClyUnknownQueryResult >> protectAccessWhile: aBlock [
	^aBlock value
]

{ #category : #'system changes' }
ClyUnknownQueryResult >> subscribe: anObject [
]

{ #category : #'system changes' }
ClyUnknownQueryResult >> unsubscribe: anObject [
]
