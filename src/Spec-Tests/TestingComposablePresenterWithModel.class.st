"
A testing composable presenter for testing of the class ComposablePresenterWithModel
"
Class {
	#name : #TestingComposablePresenterWithModel,
	#superclass : #ComposablePresenterWithModel,
	#instVars : [
		'x',
		'y'
	],
	#category : #Spec-Tests-Utils
}

{ #category : #specs }
TestingComposablePresenterWithModel class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed newColumn: [ :col |
		col newRow: [:row | row add: #x; add: #y ]];
		yourself

]

{ #category : #specs }
TestingComposablePresenterWithModel class >> open [

	<example>
	
	(self on: 1@2) openWithSpec
]

{ #category : #initialization }
TestingComposablePresenterWithModel >> initializeWidgets [

	x := self newTextInput.
	y := self newTextInput.
]

{ #category : #initialization }
TestingComposablePresenterWithModel >> modelChanged [

	x text: self model x asString.
	y text: self model y asString.	
	
]

{ #category : #specs }
TestingComposablePresenterWithModel >> title [

	^ 'Point'
]

{ #category : #accessing }
TestingComposablePresenterWithModel >> x [
	^ x
]

{ #category : #accessing }
TestingComposablePresenterWithModel >> x: anObject [
	x := anObject
]

{ #category : #accessing }
TestingComposablePresenterWithModel >> y [
	^ y
]

{ #category : #accessing }
TestingComposablePresenterWithModel >> y: anObject [
	y := anObject
]
