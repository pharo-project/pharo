*Morphic-Widgets-Windows
embeddedInMorphicWindowLabeled: labelString
	| window |
	window := (SystemWindow labelled: labelString) model: nil.
	window setStripeColorsFrom:  (self theme windowColorFor: self).
	window addMorph: self frame: (0@0 extent: 1@1).
	^ window