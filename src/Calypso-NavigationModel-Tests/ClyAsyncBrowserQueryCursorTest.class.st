Class {
	#name : #ClyAsyncBrowserQueryCursorTest,
	#superclass : #ClyBrowserItemCursorTest,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyAsyncBrowserQueryCursorTest class >> shouldInheritSelectors [ 
	^true
]

{ #category : #running }
ClyAsyncBrowserQueryCursorTest >> createQuery [
	^super createQuery semiAsync
]

{ #category : #tests }
ClyAsyncBrowserQueryCursorTest >> testHasQueryResult [

	self assert: cursor queryResult class equals: ClyAsyncBrowserQueryResultAdapter.
	self assert: cursor queryResult actualResult equals: queryResult
]
