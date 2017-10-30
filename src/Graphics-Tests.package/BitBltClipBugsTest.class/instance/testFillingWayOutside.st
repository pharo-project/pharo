tests - filling
testFillingWayOutside
	| f1 bb |
	f1 := Form extent: 100 @ 100 depth: 1.
	bb := BitBlt toForm: f1.
	bb combinationRule: 3.
	bb fillColor: Color black.
	bb destOrigin: SmallInteger maxVal squared asPoint.
	bb
		width: 100;
		height: 100.
	bb copyBits