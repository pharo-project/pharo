canvas drawing
line: pt1 to: pt2 width: w color: c
	| path |
	path := self createClosedPolygonPathFrom: {pt1 . pt2}.
		
	(self canvas setStrokePaint: c) width:w.
	self canvas drawShape: path.
	