"
I define a popover needs to be shown at the bottom of its owner.
"
Class {
	#name : #SpPopoverPositionBottom,
	#superclass : #SpPopoverPosition,
	#category : #'Spec2-Core-Support'
}

{ #category : #operations }
SpPopoverPositionBottom >> applyTo: aWidget [

	aWidget bePositionBottom
]
