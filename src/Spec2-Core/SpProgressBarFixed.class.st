"
Description
--------------------

I am a state of a ProgressBarPresenter managing the behavior of a fixed progress bar. I will have one value (between 0 and 1) and the progress will not change.

Examples
--------------------

	 ProgressBarFixed value: 0.1
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	valueHolder:		<aValueHolder> 		Value holder keeping the value I should be fixed on.

"
Class {
	#name : #SpProgressBarFixed,
	#superclass : #SpProgressBarState,
	#traits : 'SpTObservable',
	#classTraits : 'SpTObservable classTrait',
	#instVars : [
		'#value => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets-Utils'
}

{ #category : #'instance creation' }
SpProgressBarFixed class >> value: aNumber [
	^ self new
		value: aNumber;
		yourself
]

{ #category : #initialization }
SpProgressBarFixed >> initialize [
	
	self class initializeSlots: self.
	super initialize.
	value := 0
]

{ #category : #accessing }
SpProgressBarFixed >> value [
	^ value
]

{ #category : #accessing }
SpProgressBarFixed >> value: aNumber [
	value := aNumber
]

{ #category : #'api-events' }
SpProgressBarFixed >> whenValueChangedDo: aBlock [
	self property: #value whenChangedDo: aBlock
]
