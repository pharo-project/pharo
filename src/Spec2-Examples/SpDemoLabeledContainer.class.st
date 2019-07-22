"
A component that should encapsulate a form part with a specific label
"
Class {
	#name : #SpDemoLabeledContainer,
	#superclass : #SpLabelledContainer,
	#category : #'Spec2-Examples-Demo-Support'
}

{ #category : #specs }
SpDemoLabeledContainer class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #label withConstraints: [ :constraints | constraints height: self inputTextHeight ];
		add: #subwidget;
		yourself
]

{ #category : #accessing }
SpDemoLabeledContainer >> label: aString [

	label label: ' - ', aString, ' - '
]
