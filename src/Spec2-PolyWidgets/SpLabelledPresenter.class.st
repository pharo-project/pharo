"
Description
--------------------

A LabelledPresenter is a simple presenter to display an input with a label and optionnaly a description.

Example:

	self instantiate: (LabelledPresenter label: 'Email' input: self newTextInput description: 'Email adress to use to send activation and informations emails.')
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
	^ SpLayout composed
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

{ #category : #specs }
SpLabelledPresenter class >> title [
	^ 'Labelled presenter'
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
