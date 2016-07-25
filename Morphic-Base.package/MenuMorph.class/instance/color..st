accessing
color: aColor
	"Set the receiver's color. Remember the base color in the case of a gradient background."

	super color: aColor.
	self setProperty: #basicColor toValue: aColor