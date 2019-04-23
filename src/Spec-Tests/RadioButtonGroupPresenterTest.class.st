Class {
	#name : #RadioButtonGroupPresenterTest,
	#superclass : #SpecSmokeTestCase,
	#instVars : [
		'radio1',
		'radio2'
	],
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
RadioButtonGroupPresenterTest >> classToTest [
	^ RadioButtonGroupPresenter
]

{ #category : #running }
RadioButtonGroupPresenterTest >> initializeTestedInstance [
	radio1 := RadioButtonPresenter new.
	radio2 := RadioButtonPresenter new
]

{ #category : #tests }
RadioButtonGroupPresenterTest >> testAddRadioButton [
	self assert: presenter buttons isEmpty.
	presenter
		addRadioButton: radio1;
		addRadioButton: radio2.
	self assert: presenter buttons size equals: 2
]

{ #category : #tests }
RadioButtonGroupPresenterTest >> testAddingFocusOrder [
	presenter
		addRadioButton: radio1;
		addRadioButton: radio2.
	self shouldnt: [ presenter takeKeyboardFocus ] raise: Error.
	self assert: (presenter giveFocusToNextFrom: radio1)
]

{ #category : #tests }
RadioButtonGroupPresenterTest >> testDefault [
	presenter addRadioButton: radio1.
	self deny: radio1 state
]

{ #category : #tests }
RadioButtonGroupPresenterTest >> testRebuildWidget [
	presenter
		addRadioButton: radio1;
		addRadioButton: radio2.
	self shouldnt: [ presenter radio2 ] raise: Error.
	self assert: presenter buttons size == 2
]
