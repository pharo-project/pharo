"
A testing presenter for testing of the class SpPresenterWithModel

	| model presenter |
	model := SpTestingPointModel x: 1 y: 2.
	presenter := SpTestingPresenterWithModel on: model.
	presenter openWithSpec
"
Class {
	#name : #SpTestingPresenterWithModel,
	#superclass : #SpPresenterWithModel,
	#instVars : [
		'x',
		'y'
	],
	#category : #'Spec2-Tests-Utils'
}

{ #category : #specs }
SpTestingPresenterWithModel class >> defaultSpec [
	^ SpBoxLayout newHorizontal
		add: #x;
		add: #y;
		yourself
]

{ #category : #specs }
SpTestingPresenterWithModel class >> open [

	<example>
	
	(self on: 1@2) openWithSpec
]

{ #category : #initialization }
SpTestingPresenterWithModel >> initializePresenters [
	
	x := self newTextInput.
	y := self newTextInput.
]

{ #category : #initialization }
SpTestingPresenterWithModel >> modelChanged [

	x text: self model x asString.
	y text: self model y asString.	
	
]

{ #category : #api }
SpTestingPresenterWithModel >> title [

	^ 'Point'
]

{ #category : #accessing }
SpTestingPresenterWithModel >> x [
	^ x
]

{ #category : #accessing }
SpTestingPresenterWithModel >> x: anObject [
	x := anObject
]

{ #category : #accessing }
SpTestingPresenterWithModel >> y [
	^ y
]

{ #category : #accessing }
SpTestingPresenterWithModel >> y: anObject [
	y := anObject
]
