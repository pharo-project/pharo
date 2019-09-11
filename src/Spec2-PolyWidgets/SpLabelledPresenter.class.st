"
A LabelledPresenter is a simple presenter to display an input with a label and optionnaly a description.

self example.

	
"
Class {
	#name : #SpLabelledPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'input',
		'descriptionPresenter',
		'labelPresenter'
	],
	#category : #'Spec2-PolyWidgets-Labelled'
}

{ #category : #specs }
SpLabelledPresenter class >> defaultSpec [
	^ SpBoxLayout newHorizontal
		add: #labelPresenter withConstraints: [ :constraints | constraints width: self labelWidth ];
		add:
			(SpBoxLayout newVertical
				add: #input withConstraints: [ :constraints | constraints height: self inputTextHeight ];
				add: #descriptionPresenter withConstraints: [ :constraints | constraints height: self inputTextHeight ]);
		yourself
]

{ #category : #specs }
SpLabelledPresenter class >> example [
	| inputPresenter |
	inputPresenter := SpTextInputFieldPresenter new.
	(SpLabelledPresenter 
		label: 'Email' 
		input: inputPresenter
		description: 'Email adress to use to send activation and informations emails.')
		openWithSpec
	
]

{ #category : #'instance creation' }
SpLabelledPresenter class >> label: aString input: aPresenter [
	^ self label: aString input: aPresenter description: nil
]

{ #category : #'instance creation' }
SpLabelledPresenter class >> label: aString input: aPresenter description: anotherString [
	^ self new
		label: aString;
		input: aPresenter;
		description: anotherString;
		yourself
]

{ #category : #accessing }
SpLabelledPresenter >> description: aString [
	aString ifNil: [ ^ self ].
	
	descriptionPresenter := self newLabel
		label: aString;
		emphasis: #italic;
		yourself
]

{ #category : #accessing }
SpLabelledPresenter >> descriptionPresenter [
	^ descriptionPresenter
]

{ #category : #accessing }
SpLabelledPresenter >> descriptionPresenter: anObject [
	descriptionPresenter := anObject
]

{ #category : #initialization }
SpLabelledPresenter >> initializeWidgets [
	labelPresenter := self newLabel.
	descriptionPresenter :=  self newNullPresenter. "By default it is null"
]

{ #category : #accessing }
SpLabelledPresenter >> input [
	^ input
]

{ #category : #accessing }
SpLabelledPresenter >> input: anObject [
	input := anObject.
	self focusOrder add: input
]

{ #category : #accessing }
SpLabelledPresenter >> label: aString [
	labelPresenter label: aString
]

{ #category : #accessing }
SpLabelledPresenter >> labelPresenter [
	^ labelPresenter
]

{ #category : #accessing }
SpLabelledPresenter >> labelPresenter: anObject [
	labelPresenter := anObject
]
