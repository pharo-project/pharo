Class {
	#name : #ClyAsyncRawQueryCursorTest,
	#superclass : #ClyRawItemCursorTest,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyAsyncRawQueryCursorTest class >> shouldInheritSelectors [ 
	^true
]

{ #category : #running }
ClyAsyncRawQueryCursorTest >> createQuery [
	^super createQuery semiAsync
]
