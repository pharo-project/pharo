private
debugDrawAt: offset
	| canvas |
	canvas := Display getCanvas.
	canvas translateBy: offset during:[:aCanvas|
		self lineSegmentsDo:[:p1 :p2|
			aCanvas line: p1 rounded to: p2 rounded width: 1 color: Color black.
		].
	].