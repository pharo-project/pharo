"
I am a popover content presenter. 
All popovers contents need to extent me (to get some functionalies)
"
Class {
	#name : #SpPopoverContentPresenter,
	#superclass : #SpPresenter,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #api }
SpPopoverContentPresenter >> dismiss [

	^ self owner dismiss
]
