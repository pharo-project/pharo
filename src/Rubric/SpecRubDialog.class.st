Class {
	#name : #SpecRubDialog,
	#superclass : #SpPresenter,
	#instVars : [
		'service',
		'findInput',
		'findButton'
	],
	#category : #'Rubric-Editing-FindReplaceService'
}

{ #category : #specs }
SpecRubDialog class >> defaultSpec [
	<spec>
	^ SpBoxLayout newVertical
		add: #findInput;
		add: #findButton;
		yourself
]

{ #category : #accessing }
SpecRubDialog >> findButton [
	^ findButton
]

{ #category : #accessing }
SpecRubDialog >> findButton: anObject [
	findButton := anObject
]

{ #category : #accessing }
SpecRubDialog >> findInput [
	^ findInput
]

{ #category : #accessing }
SpecRubDialog >> findInput: anObject [
	findInput := anObject.
]

{ #category : #initialization }
SpecRubDialog >> initializePresenters [
	findInput := self newTextInput autoAccept: true.
	findInput whenTextChangedDo: [ self updateFindText ].
	findButton := self newButton label: 'Find'.
	findButton action: [ service find ].
	self updateFromModel
]

{ #category : #accessing }
SpecRubDialog >> service [
	^ service
]

{ #category : #accessing }
SpecRubDialog >> service: anObject [
	service := anObject
]

{ #category : #initialization }
SpecRubDialog >> setModelBeforeInitialization: aService [

	service := aService
]

{ #category : #initialization }
SpecRubDialog >> updateFindText [

	#updateFindText traceCr.
	service findText: self findInput text asString.
]

{ #category : #initialization }
SpecRubDialog >> updateFromModel [

	findInput text: self service findText.


]
