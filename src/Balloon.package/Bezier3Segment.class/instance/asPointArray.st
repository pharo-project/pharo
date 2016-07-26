converting
asPointArray
	| p |
	p := PointArray new: 4.
	p at: 1 put: start.
	p at: 2 put: via1.
	p at: 3 put: via2.
	p at: 4 put: end.
	^ p