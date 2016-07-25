drawing
frameRectangle7: aRectangle on: aCanvas
	"Draw the border for a corner radius of 7."

	|r|
	r := aRectangle insetBy: self width // 2.	
	self width odd ifTrue: [r := r insetBy: (0@0 corner: 1@1)].
	aCanvas
		line: r topLeft + (7@0) to: r topRight - (7@0) width: self width color: self color;
		line: r topRight + (-6@1) to: r topRight + (-5@1) width: self width color: self color;
		line: r topRight + (-4@2) to: r topRight + (-2@4) width: self width color: self color;
		line: r topRight + (-1@5) to: r topRight + (-1@6) width: self width color: self color;
		line: r topRight + (0@7) to: r bottomRight - (0@7) width: self width color: self color;
		line: r bottomRight - (1@6) to: r bottomRight - (1@5) width: self width color: self color;
		line: r bottomRight - (2@4) to: r bottomRight - (4@2) width: self width color: self color;
		line: r bottomRight - (5@1) to: r bottomRight - (6@1) width: self width color: self color;
		line: r bottomRight - (7@0) to: r bottomLeft + (7@0) width: self width color: self color;
		line: r bottomLeft - (-6@1) to: r bottomLeft - (-5@1) width: self width color: self color;
		line: r bottomLeft - (-4@2) to: r bottomLeft - (-2@4) width: self width color: self color;
		line: r bottomLeft - (-1@5) to: r bottomLeft - (-1@6) width: self width color: self color;
		line: r bottomLeft - (0@7) to: r topLeft + (0@7) width: self width color: self color;
		line: r topLeft + (1@6) to: r topLeft + (1@5) width: self width color: self color;
		line: r topLeft + (2@4) to: r topLeft + (4@2) width: self width color: self color;
		line: r topLeft + (5@1) to: r topLeft + (6@1) width: self width color: self color