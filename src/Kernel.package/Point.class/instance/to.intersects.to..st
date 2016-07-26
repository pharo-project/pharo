geometry
to: end1 intersects: start2 to: end2 
	"Returns true if the linesegment from start1 (=self) to end1 intersects with the segment from start2 to end2, otherwise false."
	
	| start1 sideStart sideEnd |
	start1 := self.
	(((start1 = start2 or: [ end1 = end2 ]) or: [ start1 = end2 ]) or: [ start2 = end1 ]) ifTrue: [ ^ true ].
	sideStart := start1 
		to: end1
		sideOf: start2.
	sideEnd := start1 
		to: end1
		sideOf: end2.
	sideStart = sideEnd ifTrue: [ ^ false ].
	sideStart := start2 
		to: end2
		sideOf: start1.
	sideEnd := start2 
		to: end2
		sideOf: end1.
	sideStart = sideEnd ifTrue: [ ^ false ].
	^ true