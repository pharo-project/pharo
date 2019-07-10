"
Description
--------------------

A LabelledPresenter is a simple presenter to display an input with a label and optionnaly a description.

Example:

	self instantiate: (LabelledPresenter label: 'Email' input: self newTextInput description: 'Email adress to use to send activation and informations emails.')
"
Class {
	#name : #LabelledPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'input',
		'descriptionPresenter',
		'labelPresenter'
	],
	#category : #'Spec-PolyWidgets-Labelled'
}

{ #category : #specs }
LabelledPresenter class >> defaultSpec [
	^ SpecLayout composed
		newRow: [ :row | 
			row
				newColumn: [ :c | 
					c
						newRow: [ :r | r add: #labelPresenter ] height: self inputTextHeight;
						addSpacer ]
					width: self labelWidth;
				newColumn: [ :c | 
					c
						newRow: [ :r | r add: #input ] height: self inputTextHeight;
						newRow: [ :r | r add: #descriptionPresenter ] height: self inputTextHeight ] ]
			height: self labelledPresenterHeight;
		yourself
]

{ #category : #'instance creation' }
LabelledPresenter class >> label: aString input: aPresenter [
	^ self label: aString input: aPresenter description: nil
]

{ #category : #'instance creation' }
LabelledPresenter class >> label: aString input: aPresenter description: anotherString [
	^ self new
		label: aString;
		input: aPresenter;
		description: anotherString;
		yourself
]

{ #category : #specs }
LabelledPresenter class >> title [
	^ 'Labelled presenter'
]

{ #category : #accessing }
LabelledPresenter >> description: aString [
	aString ifNil: [ ^ self ].
	
	descriptionPresenter := self newLabel
		label: aString;
		emphasis: #italic;
		yourself
]

{ #category : #accessing }
LabelledPresenter >> descriptionPresenter [
	^ descriptionPresenter
]

{ #category : #accessing }
LabelledPresenter >> descriptionPresenter: anObject [
	descriptionPresenter := anObject
]

{ #category : #initialization }
LabelledPresenter >> initializeWidgets [
	labelPresenter := self newLabel.
	descriptionPresenter :=  self newNullPresenter. "By default it is null"
]

{ #category : #accessing }
LabelledPresenter >> input [
	^ input
]

{ #category : #accessing }
LabelledPresenter >> input: anObject [
	input := anObject.
	self focusOrder add: input
]

{ #category : #accessing }
LabelledPresenter >> label: aString [
	labelPresenter label: aString
]

{ #category : #accessing }
LabelledPresenter >> labelPresenter [
	^ labelPresenter
]

{ #category : #accessing }
LabelledPresenter >> labelPresenter: anObject [
	labelPresenter := anObject
]
