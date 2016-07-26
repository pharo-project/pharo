initialization
textColor: aColor
	"Set my default text color."
	self setProperty: #textColor toValue: aColor.
	self listMorph color: aColor.