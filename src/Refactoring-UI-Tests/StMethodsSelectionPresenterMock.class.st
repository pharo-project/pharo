Class {
	#name : 'StMethodsSelectionPresenterMock',
	#superclass : 'Object',
	#instVars : [
		'title',
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

{ #category : 'initialization' }
StMethodsSelectionPresenterMock class >> title: aTitle label: aString withItems: aCollection selecting: aCollection3 [

	^ self new
		  title: aTitle
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

{ #category : 'initialization' }
StMethodsSelectionPresenterMock >> title: aTitle label: aString withItems: aOrderedCollection selecting: aOrderedCollectionToo [

	title := aTitle.
	label := 'Push down methods from a class'.
	items := aOrderedCollection.
	selecting := aOrderedCollectionToo
]
