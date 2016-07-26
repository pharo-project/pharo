private
getVertices

	smoothCurve ifFalse: [^ vertices].

	"For curves, enumerate the full set of interpolated points"
	^ Array streamContents:
		[:s | self lineSegmentsDo: [:pt1 :pt2 | s nextPut: pt1]]