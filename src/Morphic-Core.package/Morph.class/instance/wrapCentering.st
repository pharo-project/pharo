layout-properties
wrapCentering
	"Layout specific. This property describes how the rows/columns in a list-like layout should be centered.
		#topLeft - center at start of secondary direction
		#bottomRight - center at end of secondary direction
		#center - center in the middle of secondary direction
		#justified - insert extra space inbetween rows/columns
	"
	| props |
	props := self layoutProperties.
	^props ifNil:[#topLeft] ifNotNil:[props wrapCentering].