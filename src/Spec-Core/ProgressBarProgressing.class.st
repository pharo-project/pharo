"
Description
--------------------

I am a state of a ProgressBarPresenter managing the behavior of a progress bar which value can progress.

I will have a block that should return a number between 0 and 1 to get the current progress and a delay defining the refresh rate of the progress bar.

Examples
--------------------

	ProgressBarProgressing progression: [ model computationStep ] every: 0.2 seconds
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	currentValueHolder:		<aValueHolder>		Store the current value of the progress bar. We need it to be able to notify the user when it changes so that the user can register to the change.
	progression:					<aValuable>			A block returning a number between 0 and 1 defining the progress of the bar.
	refreshDelay:				<aDelay>				A delay defining the refresh rate of the bar.

"
Class {
	#name : #ProgressBarProgressing,
	#superclass : #ProgressBarState,
	#instVars : [
		'refreshDelay',
		'currentValueHolder',
		'progression'
	],
	#category : #'Spec-Core-Widgets-Utils'
}

{ #category : #'instance creation' }
ProgressBarProgressing class >> progression: aBlock every: aDelay [
	^ self new
		progression: aBlock;
		refreshDelay: aDelay;
		yourself
]

{ #category : #accessing }
ProgressBarProgressing >> currentValue [
	^ currentValueHolder value
]

{ #category : #accessing }
ProgressBarProgressing >> currentValue: aNumber [
	currentValueHolder value: aNumber
]

{ #category : #initialization }
ProgressBarProgressing >> initialize [
	super initialize.
	currentValueHolder := 0 asValueHolder
]

{ #category : #accessing }
ProgressBarProgressing >> progression [
	^ progression
]

{ #category : #accessing }
ProgressBarProgressing >> progression: aValuable [
	progression := aValuable
]

{ #category : #accessing }
ProgressBarProgressing >> refreshDelay [
	^ refreshDelay
]

{ #category : #accessing }
ProgressBarProgressing >> refreshDelay: aDelay [
	refreshDelay := aDelay
]

{ #category : #'api-events' }
ProgressBarProgressing >> whenValueChangedDo: aBlock [
	currentValueHolder whenChangedDo: aBlock
]
