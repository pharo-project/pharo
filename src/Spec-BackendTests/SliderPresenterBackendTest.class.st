Class {
	#name : #SliderPresenterBackendTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #accessing }
SliderPresenterBackendTest >> classToTest [
	^ SliderPresenter 
]

{ #category : #accessing }
SliderPresenterBackendTest >> initializeTestedInstance [
	presenter
		min: 1;
		max: 100;
		quantum: 1;
		value: 20
]

{ #category : #accessing }
SliderPresenterBackendTest >> testSmokeHorizontalTest [
	self presenter beHorizontal.
]

{ #category : #accessing }
SliderPresenterBackendTest >> testSmokeVerticalTest [
	self presenter beVertical.
	
]
