drawing-ovals
fillOval: r color: c borderWidth: borderWidth borderColor: borderColor
	"Fill the given oval."
	myCanvas
		fillOval: r
		color: (self mapColor: c)
		borderWidth: borderWidth
		borderColor: (self mapColor: borderColor)