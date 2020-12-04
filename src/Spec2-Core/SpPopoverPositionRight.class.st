"
I define a popover needs to be shown at the right of its owner.
"
Class {
	#name : #SpPopoverPositionRight,
	#superclass : #SpPopoverPosition,
	#category : #'Spec2-Core-Support'
}

{ #category : #operations }
SpPopoverPositionRight >> applyTo: aWidget [

	aWidget bePositionRight
]
