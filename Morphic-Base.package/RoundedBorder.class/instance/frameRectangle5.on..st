drawing
frameRectangle5: aRectangle on: aCanvas
	"Draw the border for a corner radius of 5."

	|r|
	r := aRectangle insetBy: self width // 2.	
	self width odd ifTrue: [r := r insetBy: (0@0 corner: 1@1)].
	aCanvas
		line: r topLeft + (5@0) to: r topRight - (5@0) width: self width color: self color;
		line: r topRight + (-4@1) to: r topRight + (-2@2) width: self width color: self color;
		line: r topRight + (-1@3) to: r topRight + (-1@4) width: self width color: self color;
		line: r topRight + (0@5) to: r bottomRight - (0@5) width: self width color: self color;
		line: r bottomRight - (1@4) to: r bottomRight - (2@2) width: self width color: self color;
		line: r bottomRight - (3@1) to: r bottomRight - (4@1) width: self width color: self color;
		line: r bottomRight - (5@0) to: r bottomLeft + (5@0) width: self width color: self color;
		line: r bottomLeft - (-4@1) to: r bottomLeft - (-3@1) width: self width color: self color;
		line: r bottomLeft - (-2@2) to: r bottomLeft - (-1@4) width: self width color: self color;
		line: r bottomLeft - (0@5) to: r topLeft + (0@5) width: self width color: self color;
		line: r topLeft + (1@4) to: r topLeft + (1@3) width: self width color: self color;
		line: r topLeft + (2@2) to: r topLeft + (4@1) width: self width color: self color