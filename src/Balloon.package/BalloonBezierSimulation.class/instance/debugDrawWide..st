private
debugDrawWide: n
	| entry minY maxY canvas curve p1 p2 entry2 y |
	curve := self class new.
	curve start: start + (0@n).
	curve via: via + (0@n).
	curve end: end + (0@n).
	entry := BalloonEdgeData new.
	entry2 := BalloonEdgeData new.
	canvas := Display getCanvas.
	minY := (start y min: end y) min: via y.
	maxY := (start y max: end y) max: via y.
	entry yValue: minY.
	entry2 yValue: minY + n.
	self stepToFirstScanLineAt: minY in: entry.
	curve stepToFirstScanLineAt: minY+n in: entry2.
	y := minY.
	1 to: n do:[:i|
		y := y + 1.
		self stepToNextScanLineAt: y in: entry.
		p1 := entry xValue @ y.
		canvas line: p1 to: p1 + (n@0) width: 1 color: Color black.
	].
	[y < maxY] whileTrue:[
		y := y + 1.
		self stepToNextScanLineAt: y in: entry.
		p2 := (entry xValue + n) @ y.
		curve stepToNextScanLineAt: y in: entry2.
		p1 := entry2 xValue @ y.
		canvas line: p1 to: p2 width: 1 color: Color black.
	].
