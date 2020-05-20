"
I define a popover needs to be shown at the top of its owner.
"
Class {
	#name : #SpPopoverPositionTop,
	#superclass : #SpPopoverPosition,
	#category : #'Spec2-Core-Support'
}

{ #category : #operations }
SpPopoverPositionTop >> applyTo: aWidget [

	aWidget bePositionTop
]
