drawing
frameRectangle1: aRectangle on: aCanvas
	"Draw the border for a corner radius of 1."

	|r|
	r := aRectangle insetBy: self width // 2.	
	self width odd ifTrue: [r := r insetBy: (0@0 corner: 1@1)].
	aCanvas
		line: r topLeft + (1@0) to: r topRight - (1@0) width: self width color: self color;
		line: r topRight + (0@1) to: r bottomRight - (0@1) width: self width color: self color;
		line: r bottomRight - (1@0) to: r bottomLeft + (1@0) width: self width color: self color;
		line: r bottomLeft - (0@1) to: r topLeft + (0@1) width: self width color: self color