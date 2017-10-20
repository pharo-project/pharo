tests - drawing
testDrawingWayOutside
	| f1 bb f2 |
	f1 := Form extent: 100 @ 100 depth: 1.
	f2 := Form extent: 100 @ 100 depth: 1.
	bb := BitBlt toForm: f1.
	bb combinationRule: 3.
	bb sourceForm: f2.
	bb destOrigin: SmallInteger maxVal squared asPoint.
	bb
		width: 100;
		height: 100.
	bb copyBits