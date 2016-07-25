tests
testTransformReturnEmptyRectangleWhenNotPossibleToPlace
	"the transform: operation is transforming a layoutFrame relatively to the reference rectangle: basically you transform to obtain a subrectangle area inside the reference rectangle"

	| lf rectangle refRectangle |
	lf := LayoutFrame new
			leftOffset: 10;
			topOffset: 10;
			rightOffset: -10;
			bottomOffset: -10;
			yourself.
	refRectangle := 0@0 extent: 10@10.
	rectangle := lf transform: refRectangle.
	self assert: rectangle width = 0.
	self assert: rectangle height = 0.