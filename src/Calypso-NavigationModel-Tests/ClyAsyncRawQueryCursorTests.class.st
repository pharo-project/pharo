Class {
	#name : #ClyAsyncRawQueryCursorTests,
	#superclass : #ClyRawItemCursorTests,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyAsyncRawQueryCursorTests class >> shouldInheritSelectors [ 
	^true
]

{ #category : #running }
ClyAsyncRawQueryCursorTests >> createQuery [
	^super createQuery semiAsync
]
