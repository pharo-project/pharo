Class {
	#name : #RadioButtonGroupPresenterTest,
	#superclass : #SpecTestCase,
	#instVars : [
		'radio1',
		'radio2'
	],
	#category : 'Spec-Tests-Core-Widgets'
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
	self assert: testedInstance buttons size == 0.
	testedInstance
		addRadioButton: radio1;
		addRadioButton: radio2.
	self assert: testedInstance buttons size == 2
]

{ #category : #tests }
RadioButtonGroupPresenterTest >> testAddingFocusOrder [
	testedInstance
		addRadioButton: radio1;
		addRadioButton: radio2.
	self shouldnt: [ testedInstance takeKeyboardFocus ] raise: Error.
	self assert: (testedInstance giveFocusToNextFrom: radio1)
]

{ #category : #tests }
RadioButtonGroupPresenterTest >> testCurrentActivated [
	testedInstance addRadioButton: radio1.
	self assert: testedInstance currentActivated equals: nil.
	radio1 click.
	self assert: testedInstance currentActivated equals: radio1
]

{ #category : #tests }
RadioButtonGroupPresenterTest >> testDefault [
	testedInstance addRadioButton: radio1.
	self assert: radio1 state == false
]

{ #category : #tests }
RadioButtonGroupPresenterTest >> testLinkRadioButton [
	testedInstance
		addRadioButton: radio1;
		addRadioButton: radio2.
	self assert: radio1 state == false.
	self assert: radio2 state == false.
	radio1 click.
	self assert: radio1 state == true.
	self assert: radio2 state == false.
	radio2 click.
	self assert: radio1 state == false.
	self assert: radio2 state == true
]

{ #category : #tests }
RadioButtonGroupPresenterTest >> testRebuildWidget [
	testedInstance
		addRadioButton: radio1;
		addRadioButton: radio2.
	self shouldnt: [ testedInstance radio2 ] raise: Error.
	self assert: testedInstance buttons size == 2
]
