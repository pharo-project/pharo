drawing
line: pt1 to: pt2 width: w color: c
	"Draw a line using the given width and color"
	myCanvas
		line: pt1
		to: pt2
		width: w
		color: (self mapColor: c).