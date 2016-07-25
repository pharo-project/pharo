border-styles-buttons
dropListFocusBoundsFor: aDropList
	"Answer the bounds for drawing the focus indication for the
	given drop list."

	^(aDropList innerBounds
		insetBy: (0@0 corner: aDropList buttonMorph width@0))
		insetBy: aDropList layoutInset