"
This demo shows differenty styles of radio buttons.
"
Class {
	#name : #SpDemoRadioButtonsPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'radioButtonExample'
	],
	#category : #'Spec2-Examples-Demo-Checkboxes'
}

{ #category : #specs }
SpDemoRadioButtonsPresenter class >> defaultSpec [
	^ SpBoxLayout newLeftToRight
		add: #radioButtonExample;
		yourself
]

{ #category : #initialization }
SpDemoRadioButtonsPresenter >> initializePresenters [
	radioButtonExample := self instantiate: SpRadioButtonExample
]
