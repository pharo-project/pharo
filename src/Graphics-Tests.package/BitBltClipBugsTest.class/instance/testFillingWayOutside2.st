tests - filling
testFillingWayOutside2
	| f1 bb |
	f1 := Form extent: 100 @ 100 depth: 1.
	bb := BitBlt toForm: f1.
	bb combinationRule: 3.
	bb fillColor: Color black.
	bb destOrigin: 0 @ 0.
	bb
		width: SmallInteger maxVal squared;
		height: SmallInteger maxVal squared.
	bb copyBits