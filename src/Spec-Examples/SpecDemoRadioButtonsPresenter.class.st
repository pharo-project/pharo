"
This demo shows differenty styles of radio buttons.
"
Class {
	#name : #SpecDemoRadioButtonsPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'radioButtonExample'
	],
	#category : #'Spec-Examples-Demo-Checkboxes'
}

{ #category : #specs }
SpecDemoRadioButtonsPresenter class >> defaultSpec [
	^ SpecBoxLayout newHorizontal
		add: #radioButtonExample;
		yourself
]

{ #category : #initialization }
SpecDemoRadioButtonsPresenter >> initializeWidgets [
	radioButtonExample := self instantiate: RadioButtonExample
]
