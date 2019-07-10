"
Description
--------------------

I am a state of a ProgressBarPresenter managing the behavior of a indeterminated progress bar. I will have no value and display a bar going back and forth.

Examples
--------------------

	ProgressBarIndeterminated new
"
Class {
	#name : #ProgressBarIndeterminated,
	#superclass : #ProgressBarState,
	#category : #'Spec-Core-Widgets-Utils'
}

{ #category : #'api-events' }
ProgressBarIndeterminated >> whenValueChangedDo: aBlock [
	"Value cannot change in indeterminate state"
]
