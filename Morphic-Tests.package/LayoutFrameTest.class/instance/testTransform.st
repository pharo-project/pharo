tests
testTransform
	"the transform: operation is transforming a layoutFrame relatively to the reference rectangle: basically you transform to obtain a subrectangle area inside the reference rectangle"

	| lf rectangle |
	lf := LayoutFrame new
			leftOffset: 10;
			topOffset: 10;
			rightOffset: -10;
			bottomOffset: -10;
			yourself.
	rectangle := lf layout: nil in: (50@10 corner: 150@70).
	self assert: (60@20 corner: 140@60) = rectangle