Class {
	#name : 'StSelectClassAndMethodsPresenterMock',
	#superclass : 'Object',
	#instVars : [
		'title',
		'label',
		'dropLabel',
		'items',
		'selecting',
		'dropItems',
		'acceptBlock'
	],
	#category : 'Refactoring-UI-Tests-Mocks',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Mocks'
}

{ #category : 'testing' }
StSelectClassAndMethodsPresenterMock class >> cancelled [

	^ false
]

{ #category : 'initialization' }
StSelectClassAndMethodsPresenterMock class >> label: aString dropLabel: aString2 withItems: aCollection selecting: aCollection4 dropItems: aCollection5 acceptBlock: aFullBlockClosure [

	^ self new
		label: aString
		dropLabel: aString2
		withItems: aCollection5
		selecting: aCollection4
		dropItems: aCollection
		acceptBlock: aFullBlockClosure
]

{ #category : 'accessing' }
StSelectClassAndMethodsPresenterMock class >> selectedClass [
	^ nil
]

{ #category : 'initialization' }
StSelectClassAndMethodsPresenterMock class >> title: aTitle label: aString dropLabel: aString2 withItems: aCollection selecting: aCollection4 dropItems: aCollection5 acceptBlock: aFullBlockClosure [

	^ self new
		  title: aTitle
		  label: aString
		  dropLabel: aString2
		  withItems: aCollection5
		  selecting: aCollection4
		  dropItems: aCollection
		  acceptBlock: aFullBlockClosure
]

{ #category : 'testing' }
StSelectClassAndMethodsPresenterMock >> cancelled [
	^ false
]

{ #category : 'initialization' }
StSelectClassAndMethodsPresenterMock >> label: aString dropLabel: aString2 withItems: aCollection selecting: aCollection4 dropItems: aCollection5 acceptBlock: aFullBlockClosure [ 
	label := aString.
	dropLabel := aString2.
	items := aCollection.
	selecting := aCollection4.
	dropItems := aCollection4.
	acceptBlock := aFullBlockClosure.
	acceptBlock value: self selectedClass value: self selectedMethods
]

{ #category : 'initialization' }
StSelectClassAndMethodsPresenterMock >> selectedClass [
	^ items first
]

{ #category : 'initialization' }
StSelectClassAndMethodsPresenterMock >> selectedMethods [
	^ selecting
]

{ #category : 'initialization' }
StSelectClassAndMethodsPresenterMock >> title: aTitle label: aString dropLabel: aString2 withItems: aCollection selecting: aCollection4 dropItems: aCollection5 acceptBlock: aFullBlockClosure [

	title := aTitle.
	label := aString. 
	dropLabel := aString2.
	items := aCollection.
	selecting := aCollection4.
	dropItems := aCollection4.
	acceptBlock := aFullBlockClosure.
	acceptBlock value: self selectedClass value: self selectedMethods
]
