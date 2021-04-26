Class {
	#name : #ClyBrowserQueryResultTest,
	#superclass : #ClyQueryResultTestCase,
	#instVars : [
		'cursor',
		'items'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyBrowserQueryResultTest class >> isAbstract [
	^self == ClyBrowserQueryResultTest 
]

{ #category : #helpers }
ClyBrowserQueryResultTest >> assertItems: itemValueWithDepthArray [

	self 
		assertItems: itemValueWithDepthArray 
		withValue: [:item | self meaningfullObjectToAssert: item]
]

{ #category : #helpers }
ClyBrowserQueryResultTest >> assertItems: itemValueWithDepthArray withValue: aBlockToGetValue [
	| i actualItem valueOfItem |
	items := queryResult items.
	self assert: queryResult size equals: itemValueWithDepthArray size / 2.
	i := 1.
	itemValueWithDepthArray pairsDo: [ :expectedValue :expectedDepth |
		actualItem := items at: i.
		valueOfItem := aBlockToGetValue value: actualItem.
		self assert: valueOfItem equals: expectedValue.
		self assert: actualItem depth equals: expectedDepth.
		i := i + 1]
]

{ #category : #helpers }
ClyBrowserQueryResultTest >> meaningfullObjectToAssert: resultItem [
	^resultItem actualObject
]

{ #category : #tests }
ClyBrowserQueryResultTest >> testOpeningBrowserCursor [
	
	| itemObserver |
	itemObserver := ClyItemObserverExample new.
	cursor := queryResult openBrowserCursorFor: itemObserver.
	
	self assert: cursor class equals: ClyBrowserQueryCursor.
	self assert: cursor queryResult equals: queryResult.
	self assert: cursor itemObserver equals: itemObserver.
	queryResult hasSubscriber: itemObserver
]
