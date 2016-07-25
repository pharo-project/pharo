accessing
font: aFont
	"Set the label font"

	font == aFont ifTrue: [^self].
	font := aFont.
	self updateFont