drawing
line: pt1 to: pt2 width: w color: c
	"Draw a line from pt1 to: pt2"

	(aaLevel = 1 and: [self ifNoTransformWithIn:(pt1 rectangle: pt2)])
		ifTrue:[^super line: pt1 to: pt2 width: w color: c].
	^self drawPolygon: (Array with: pt1 with: pt2)
		color: c
		borderWidth: w
		borderColor: c