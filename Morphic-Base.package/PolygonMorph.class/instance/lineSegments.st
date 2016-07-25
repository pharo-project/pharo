private
lineSegments
	| lineSegments |
	lineSegments := OrderedCollection new.
	self lineSegmentsDo: [:p1 :p2 | lineSegments addLast: (Array with: p1 with: p2)].
	^ lineSegments