Class {
	#name : #SliderPresenterTest,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
SliderPresenterTest >> classToTest [
	^ SliderPresenter
]

{ #category : #running }
SliderPresenterTest >> initMinMax [
 testedInstance min: 0;
		max: 100
]

{ #category : #tests }
SliderPresenterTest >> testAbsoluteValue [
	self initMinMax.
	testedInstance absoluteValue: 0.5.
	self assert: testedInstance value identicalTo: 50
]

{ #category : #tests }
SliderPresenterTest >> testReset [
	self initMinMax.
	testedInstance
		value: 50;
		reset.
	self assert: testedInstance value identicalTo: 0
]
