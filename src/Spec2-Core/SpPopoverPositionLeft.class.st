"
I define a popover needs to be shown at the left of its owner.
"
Class {
	#name : #SpPopoverPositionLeft,
	#superclass : #SpPopoverPosition,
	#category : #'Spec2-Core-Support'
}

{ #category : #operations }
SpPopoverPositionLeft >> applyTo: aWidget [

	aWidget bePositionLeft
]
