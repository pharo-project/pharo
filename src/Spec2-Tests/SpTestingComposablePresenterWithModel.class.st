"
A testing composable presenter for testing of the class ComposablePresenterWithModel
"
Class {
	#name : #SpTestingComposablePresenterWithModel,
	#superclass : #SpPresenterWithModel,
	#instVars : [
		'x',
		'y'
	],
	#category : #'Spec2-Tests-Utils'
}

{ #category : #specs }
SpTestingComposablePresenterWithModel class >> defaultSpec [
	^ SpBoxLayout newHorizontal
		add: #x;
		add: #y;
		yourself
]

{ #category : #specs }
SpTestingComposablePresenterWithModel class >> open [

	<example>
	
	(self on: 1@2) openWithSpec
]

{ #category : #initialization }
SpTestingComposablePresenterWithModel >> initializeWidgets [

	x := self newTextInput.
	y := self newTextInput.
]

{ #category : #initialization }
SpTestingComposablePresenterWithModel >> modelChanged [

	x text: self model x asString.
	y text: self model y asString.	
	
]

{ #category : #api }
SpTestingComposablePresenterWithModel >> title [

	^ 'Point'
]

{ #category : #accessing }
SpTestingComposablePresenterWithModel >> x [
	^ x
]

{ #category : #accessing }
SpTestingComposablePresenterWithModel >> x: anObject [
	x := anObject
]

{ #category : #accessing }
SpTestingComposablePresenterWithModel >> y [
	^ y
]

{ #category : #accessing }
SpTestingComposablePresenterWithModel >> y: anObject [
	y := anObject
]
