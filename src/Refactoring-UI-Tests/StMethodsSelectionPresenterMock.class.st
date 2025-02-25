Class {
	#name : 'StMethodsSelectionPresenterMock',
	#superclass : 'Object',
	#instVars : [
		'label',
		'items',
		'selecting'
	],
	#category : 'Refactoring-UI-Tests-Mocks',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Mocks'
}

{ #category : 'initialization' }
StMethodsSelectionPresenterMock class >> label: aString withItems: aCollection selecting: aCollection3 [

	^ self new
		label: aString
		withItems: aCollection
		selecting: aCollection3
]

{ #category : 'testing' }
StMethodsSelectionPresenterMock >> cancelled [

	^ false
]

{ #category : 'initialization' }
StMethodsSelectionPresenterMock >> label: aString withItems: aOrderedCollection selecting: aOrderedCollectionToo [
		
	label := 'Push down methods from a class'.
	items := aOrderedCollection .
	selecting := aOrderedCollectionToo .
]

{ #category : 'accessing' }
StMethodsSelectionPresenterMock >> selectedItems [

	^ selecting 
]
