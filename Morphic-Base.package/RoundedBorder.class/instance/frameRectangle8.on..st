drawing
frameRectangle8: aRectangle on: aCanvas
	"Draw the border for a corner radius of 8."

	|r|
	r := aRectangle insetBy: self width // 2.	
	self width odd ifTrue: [r := r insetBy: (0@0 corner: 1@1)].
	aCanvas
		line: r topLeft + (8@0) to: r topRight - (8@0) width: self width color: self color;
		line: r topRight + (-7@1) to: r topRight + (-6@1) width: self width color: self color;
		line: r topRight + (-5@2) to: r topRight + (-2@5) width: self width color: self color;
		line: r topRight + (-1@6) to: r topRight + (-1@7) width: self width color: self color;
		line: r topRight + (0@8) to: r bottomRight - (0@8) width: self width color: self color;
		line: r bottomRight - (1@7) to: r bottomRight - (1@6) width: self width color: self color;
		line: r bottomRight - (2@5) to: r bottomRight - (5@2) width: self width color: self color;
		line: r bottomRight - (6@1) to: r bottomRight - (7@1) width: self width color: self color;
		line: r bottomRight - (8@0) to: r bottomLeft + (8@0) width: self width color: self color;
		line: r bottomLeft - (-7@1) to: r bottomLeft - (-6@1) width: self width color: self color;
		line: r bottomLeft - (-5@2) to: r bottomLeft - (-2@5) width: self width color: self color;
		line: r bottomLeft - (-1@6) to: r bottomLeft - (-1@7) width: self width color: self color;
		line: r bottomLeft - (0@8) to: r topLeft + (0@8) width: self width color: self color;
		line: r topLeft + (1@7) to: r topLeft + (1@6) width: self width color: self color;
		line: r topLeft + (2@5) to: r topLeft + (5@2) width: self width color: self color;
		line: r topLeft + (6@1) to: r topLeft + (7@1) width: self width color: self color