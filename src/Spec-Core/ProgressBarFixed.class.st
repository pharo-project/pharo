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
	#name : #ProgressBarFixed,
	#superclass : #ProgressBarState,
	#instVars : [
		'valueHolder'
	],
	#category : #'Spec-Core-Widgets-Utils'
}

{ #category : #'instance creation' }
ProgressBarFixed class >> value: aNumber [
	^ self new
		value: aNumber;
		yourself
]

{ #category : #initialization }
ProgressBarFixed >> initialize [
	super initialize.
	valueHolder := 0 asValueHolder
]

{ #category : #accessing }
ProgressBarFixed >> value [
	^ valueHolder value
]

{ #category : #accessing }
ProgressBarFixed >> value: aNumber [
	valueHolder value: aNumber
]

{ #category : #enumerating }
ProgressBarFixed >> whenValueChangedDo: aBlock [
	valueHolder whenChangedDo: aBlock
]
