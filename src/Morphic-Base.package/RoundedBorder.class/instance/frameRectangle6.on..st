drawing
frameRectangle6: aRectangle on: aCanvas
	"Draw the border for a corner radius of 6."

	|r|
	r := aRectangle insetBy: self width // 2.	
	self width odd ifTrue: [r := r insetBy: (0@0 corner: 1@1)].
	aCanvas
		line: r topLeft + (6@0) to: r topRight - (6@0) width: self width color: self color;
		line: r topRight + (-5@1) to: r topRight + (-3@2) width: self width color: self color;
		line: r topRight + (-2@3) to: r topRight + (-2@3) width: self width color: self color;
		line: r topRight + (-1@4) to: r topRight + (-1@5) width: self width color: self color;
		line: r topRight + (0@6) to: r bottomRight - (0@6) width: self width color: self color;
		line: r bottomRight - (1@5) to: r bottomRight - (2@3) width: self width color: self color;
		line: r bottomRight - (3@2) to: r bottomRight - (4@1) width: self width color: self color;
		line: r bottomRight - (5@1) to: r bottomRight - (6@0) width: self width color: self color;
		line: r bottomRight - (7@0) to: r bottomLeft + (6@0) width: self width color: self color;
		line: r bottomLeft - (-5@1) to: r bottomLeft - (-4@1) width: self width color: self color;
		line: r bottomLeft - (-3@2) to: r bottomLeft - (-3@2) width: self width color: self color;
		line: r bottomLeft - (-2@3) to: r bottomLeft - (-1@5) width: self width color: self color;
		line: r bottomLeft - (0@6) to: r topLeft + (0@6) width: self width color: self color;
		line: r topLeft + (1@5) to: r topLeft + (1@4) width: self width color: self color;
		line: r topLeft + (2@3) to: r topLeft + (2@3) width: self width color: self color;
		line: r topLeft + (3@2) to: r topLeft + (5@1) width: self width color: self color