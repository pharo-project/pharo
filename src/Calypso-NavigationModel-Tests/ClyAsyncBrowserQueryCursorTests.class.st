Class {
	#name : #ClyAsyncBrowserQueryCursorTests,
	#superclass : #ClyBrowserItemCursorTests,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyAsyncBrowserQueryCursorTests class >> shouldInheritSelectors [ 
	^true
]

{ #category : #running }
ClyAsyncBrowserQueryCursorTests >> createQuery [
	^super createQuery semiAsync
]

{ #category : #tests }
ClyAsyncBrowserQueryCursorTests >> testHasQueryResult [

	self assert: cursor queryResult class equals: ClyAsyncBrowserQueryResultAdapter.
	self assert: cursor queryResult actualResult equals: queryResult
]
