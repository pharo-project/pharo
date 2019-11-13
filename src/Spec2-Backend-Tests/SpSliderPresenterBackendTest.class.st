Class {
	#name : #SpSliderPresenterBackendTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpSliderPresenterBackendTest >> classToTest [
	^ SpSliderPresenter 
]

{ #category : #accessing }
SpSliderPresenterBackendTest >> initializeTestedInstance [
	presenter
		min: 1;
		max: 100;
		quantum: 1;
		value: 20
]

{ #category : #accessing }
SpSliderPresenterBackendTest >> testSmokeHorizontalTest [
	self presenter beHorizontal.
]

{ #category : #accessing }
SpSliderPresenterBackendTest >> testSmokeVerticalTest [
	self presenter beVertical.
	
]
