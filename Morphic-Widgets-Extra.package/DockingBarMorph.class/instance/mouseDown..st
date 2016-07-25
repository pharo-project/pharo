events-processing
mouseDown: anEvent
	"Release the mouse focus if clicked outside the receiver."

	(self fullContainsPoint: anEvent position)
		ifFalse: [anEvent hand releaseMouseFocus: self].
	^super mouseDown: anEvent