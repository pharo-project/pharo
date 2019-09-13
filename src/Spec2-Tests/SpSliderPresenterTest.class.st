Class {
	#name : #SpSliderPresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpSliderPresenterTest >> classToTest [
	^ SpSliderPresenter
]

{ #category : #running }
SpSliderPresenterTest >> initMinMax [
 presenter min: 0;
		max: 100
]

{ #category : #tests }
SpSliderPresenterTest >> testAbsoluteValue [
	self initMinMax.
	presenter absoluteValue: 0.5.
	self assert: presenter value == 50
]

{ #category : #tests }
SpSliderPresenterTest >> testAbsoluteValueToValueScales [
	self initMinMax.
	self assert:  (presenter absoluteValueToValue: 0.5) equals: 50
]

{ #category : #tests }
SpSliderPresenterTest >> testReset [
	self initMinMax.
	presenter
		value: 50;
		reset.
	self assert: presenter value == 0
]

{ #category : #tests }
SpSliderPresenterTest >> testValueToAbsoluteValueScales [
	self initMinMax.
	self assert:  (presenter valueToAbsoluteValue: 50) equals: 0.5
]
