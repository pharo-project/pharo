"
Demo page for SpecDemoRadioButtonsPresenter.
"
Class {
	#name : #SpDemoRadioButtonsPage,
	#superclass : #SpDemoPage,
	#category : #'Spec2-Examples-Demo-Checkboxes'
}

{ #category : #initialization }
SpDemoRadioButtonsPage class >> pageName [
	^ 'Radio buttons'
]

{ #category : #initialization }
SpDemoRadioButtonsPage class >> priority [
	^ 32
]

{ #category : #initialization }
SpDemoRadioButtonsPage >> pageClass [
	^ SpDemoRadioButtonsPresenter
]
