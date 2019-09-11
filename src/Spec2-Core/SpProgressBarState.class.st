"
Description
--------------------

I am an abstract class the is the root of a Strategy Design Pattern. My subclasses should have the state of different kind of progress bar. 

My subclasses are used by the ProgressBarPresenter.

"
Class {
	#name : #SpProgressBarState,
	#superclass : #Object,
	#category : #'Spec2-Core-Widgets-Utils'
}

{ #category : #'api-events' }
SpProgressBarState >> whenValueChangedDo: aBlock [
	"My subclasses should fire the block when the current value changes."

	self subclassResponsibility
]
