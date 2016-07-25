geometry
intersects: aRectangle 
	"Answer whether any of my segments intersects aRectangle, which is in World coordinates."
	| rect |
	(super intersects: aRectangle) ifFalse: [ ^false ].
	rect := self bounds: aRectangle in: self world.
	self
		lineSegmentsDo: [:p1 :p2 | (rect intersectsLineFrom: p1 to: p2)
				ifTrue: [^ true]].
	^ false