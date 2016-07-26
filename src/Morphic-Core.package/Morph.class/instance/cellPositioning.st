layout-properties
cellPositioning
	"Layout specific. This property describes how the receiver should be layed out in its owner when the bounds of the cell assigned to the receiver do not exactly match its bounds. Possible values are:
		#topLeft, #topRight, #bottomLeft, #bottomRight, #topCenter, #leftCenter, #rightCenter, #bottomCenter, #center 
	which align the receiver's bounds with the cell at the given point."
	| props |
	props := self layoutProperties.
	^props ifNil:[#center] ifNotNil:[props cellPositioning].