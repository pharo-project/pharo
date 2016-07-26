drawing
frameRectangle3: aRectangle on: aCanvas
	"Draw the border for a corner radius of 3."

	|r|
	r := aRectangle insetBy: self width // 2.	
	self width odd ifTrue: [r := r insetBy: (0@0 corner: 1@1)].
	aCanvas
		line: r topLeft + (3@0) to: r topRight - (3@0) width: self width color: self color;
		line: r topRight + (-2@1) to: r topRight + (-1@1) width: self width color: self color;
		line: r topRight + (-1@1) to: r topRight + (-1@2) width: self width color: self color;
		line: r topRight + (0@3) to: r bottomRight - (0@3) width: self width color: self color;
		line: r bottomRight - (1@2) to: r bottomRight - (1@1) width: self width color: self color;
		line: r bottomRight - (1@1) to: r bottomRight - (2@1) width: self width color: self color;
		line: r bottomRight - (3@0) to: r bottomLeft + (3@0) width: self width color: self color;
		line: r bottomLeft - (-2@1) to: r bottomLeft - (-1@1) width: self width color: self color;
		line: r bottomLeft - (-1@1) to: r bottomLeft - (-1@2) width: self width color: self color;
		line: r bottomLeft - (0@3) to: r topLeft + (0@3) width: self width color: self color;
		line: r topLeft + (1@2) to: r topLeft + (1@1) width: self width color: self color;
		line: r topLeft + (1@1) to: r topLeft + (2@1) width: self width color: self color