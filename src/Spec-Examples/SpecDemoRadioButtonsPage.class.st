"
Demo page for SpecDemoRadioButtonsPresenter.
"
Class {
	#name : #SpecDemoRadioButtonsPage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo-Checkboxes'
}

{ #category : #initialization }
SpecDemoRadioButtonsPage class >> pageName [
	^ 'Radio buttons'
]

{ #category : #initialization }
SpecDemoRadioButtonsPage class >> priority [
	^ 32
]

{ #category : #initialization }
SpecDemoRadioButtonsPage >> pageClass [
	^ SpecDemoRadioButtonsPresenter
]
