Class {
	#name : #ClyBrowserQueryResultTestCase,
	#superclass : #ClyQueryResultTestCase,
	#instVars : [
		'cursor',
		'items',
		'actualObjects'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyBrowserQueryResultTestCase class >> isAbstract [
	^self == ClyBrowserQueryResultTestCase 
]

{ #category : #helpers }
ClyBrowserQueryResultTestCase >> assertItems: itemValueWithDepthArray [

	self 
		assertItems: itemValueWithDepthArray 
		withValue: [:item | self meaningfullObjectToAssert: item]
]

{ #category : #helpers }
ClyBrowserQueryResultTestCase >> assertItems: itemValueWithDepthArray withValue: aBlockToGetValue [
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
ClyBrowserQueryResultTestCase >> meaningfullObjectToAssert: resultItem [
	^resultItem actualObject
]

{ #category : #tests }
ClyBrowserQueryResultTestCase >> testOpeningBrowserCursor [
	
	| itemObserver |
	itemObserver := ClyItemObserverExample new.
	cursor := queryResult openBrowserCursorFor: itemObserver.
	
	self assert: cursor class equals: ClyBrowserQueryCursor.
	self assert: cursor queryResult equals: queryResult.
	self assert: cursor itemObserver equals: itemObserver.
	queryResult hasSubscriber: itemObserver
]
