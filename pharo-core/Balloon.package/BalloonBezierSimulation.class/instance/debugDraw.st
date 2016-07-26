private
debugDraw
	| entry minY maxY lX lY canvas |
	entry := BalloonEdgeData new.
	canvas := Display getCanvas.
	minY := (start y min: end y) min: via y.
	maxY := (start y max: end y) max: via y.
	entry yValue: minY.
	self stepToFirstScanLineAt: minY in: entry.
	lX := entry xValue.
	lY := entry yValue.
	minY+1 to: maxY do:[:y|
		self stepToNextScanLineAt: y in: entry.
		canvas line: lX@lY to: entry xValue @ y width: 2 color: Color black.
		lX := entry xValue.
		lY := y.
	].
