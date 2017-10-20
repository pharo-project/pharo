tests - drawing
testDrawingWayOutside2
	| f1 bb f2 |
	f1 := Form extent: 100 @ 100 depth: 1.
	f2 := Form extent: 100 @ 100 depth: 1.
	bb := BitBlt toForm: f1.
	bb combinationRule: 3.
	bb sourceForm: f2.
	bb destOrigin: 0 @ 0.
	bb
		width: SmallInteger maxVal squared;
		height: SmallInteger maxVal squared.
	bb copyBits