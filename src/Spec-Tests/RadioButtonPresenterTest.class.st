Class {
	#name : #RadioButtonPresenterTest,
	#superclass : #SpecTestCase,
	#category : 'Spec-Tests-Core-Widgets'
}

{ #category : #running }
RadioButtonPresenterTest >> classToTest [
	^  RadioButtonPresenter
]

{ #category : #tests }
RadioButtonPresenterTest >> testCanDeselectByClick [
	self deny: testedInstance canDeselectByClick.
	testedInstance click.
	self assert: testedInstance state.
	testedInstance click.
	self assert: testedInstance state.
	testedInstance canDeselectByClick: true.
	testedInstance click.
	self deny: testedInstance state
]

{ #category : #tests }
RadioButtonPresenterTest >> testToggleState [
	self deny: testedInstance state.
	testedInstance toggleState.
	self assert: testedInstance state
]
