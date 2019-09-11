"
I represent method group based on async method query.
I am decorated in the browser with animation indicating background query processing.

My instances are usually created from another method group using converting method: 

	aMethodGroup asAsyncQueryGroup

I hold reference to async query result. So it is kept in memory together with my instance.

Internal Representation and Key Implementation Points.

    Instance Variables
	asyncQueryResult:		<ClyAsyncQueryResult>

"
Class {
	#name : #ClyAsyncMethodGroup,
	#superclass : #ClyMethodGroup,
	#instVars : [
		'asyncQueryResult'
	],
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'item preparation' }
ClyAsyncMethodGroup class >> prepareCalypsoItemCopy: aBrowserItem [

	| groupCopy |
	groupCopy := aBrowserItem actualObject copy.
	groupCopy asyncQueryResult: nil.
	aBrowserItem actualObject: groupCopy
]

{ #category : #testing }
ClyAsyncMethodGroup >> asAsyncQueryGroup [
	^self
]

{ #category : #accessing }
ClyAsyncMethodGroup >> asyncQueryResult [
	^ asyncQueryResult ifNil: [ asyncQueryResult := self loadAsyncQueryResult ]
]

{ #category : #accessing }
ClyAsyncMethodGroup >> asyncQueryResult: anObject [
	asyncQueryResult := anObject
]

{ #category : #decoration }
ClyAsyncMethodGroup >> decorateOwnBrowserItem: myItem [
	super decorateOwnBrowserItem: myItem.
	
	self asyncQueryResult isBuilt ifFalse: [  myItem markWith: ClyBackgroundProcessingTag]
]

{ #category : #testing }
ClyAsyncMethodGroup >> dependsOnMethod: aMethod [

	"If query result are not built yet we are depending on any possible method.
	And when result is done we can check actual dependency"
	self asyncQueryResult isBuilt ifFalse: [ ^true ].
	
	^super dependsOnMethod: aMethod
]

{ #category : #testing }
ClyAsyncMethodGroup >> includesMethod: aMethod [
	self asyncQueryResult isBuilt ifFalse: [ ^false ].
	^super includesMethod: aMethod
]

{ #category : #accessing }
ClyAsyncMethodGroup >> loadAsyncQueryResult [

	^methodQuery execute
]
