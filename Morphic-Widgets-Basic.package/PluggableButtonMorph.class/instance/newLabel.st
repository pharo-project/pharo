accessing
newLabel
	"Answer a new label for the receiver."
	|lbl|
	lbl := self theme buttonLabelFor: self.
	font ifNotNil: [lbl font: font.
		lbl extent: lbl optimalExtent ].
	^lbl