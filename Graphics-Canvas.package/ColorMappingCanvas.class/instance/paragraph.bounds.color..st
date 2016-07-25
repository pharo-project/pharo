drawing
paragraph: paragraph bounds: bounds color: c
	"Draw the given paragraph"
	myCanvas
		paragraph: paragraph
		bounds: bounds
		color: (self mapColor: c)