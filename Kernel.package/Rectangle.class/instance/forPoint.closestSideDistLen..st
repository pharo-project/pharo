rectangle functions
forPoint: aPoint closestSideDistLen: sideDistLenBlock 
	"Evaluate the block with my side (symbol) closest to aPoint,
		the approx distance of aPoint from that side, and
		the length of the side (or 0 if aPoint is beyond the side)"
	| side |
	side := self sideNearestTo: aPoint.
	side == #right ifTrue: 
		[ ^ sideDistLenBlock 
			value: side
			value: (self right - aPoint x) abs
			value: ((aPoint y 
					between: self top
					and: self bottom) 
					ifTrue: [ self height ]
					ifFalse: [ 0 ]) ].
	side == #left ifTrue: 
		[ ^ sideDistLenBlock 
			value: side
			value: (self left - aPoint x) abs
			value: ((aPoint y 
					between: self top
					and: self bottom) 
					ifTrue: [ self height ]
					ifFalse: [ 0 ]) ].
	side == #bottom ifTrue: 
		[ ^ sideDistLenBlock 
			value: side
			value: (self bottom - aPoint y) abs
			value: ((aPoint x 
					between: self left
					and: self right) 
					ifTrue: [ self width ]
					ifFalse: [ 0 ]) ].
	side == #top ifTrue: 
		[ ^ sideDistLenBlock 
			value: side
			value: (self top - aPoint y) abs
			value: ((aPoint x 
					between: self left
					and: self right) 
					ifTrue: [ self width ]
					ifFalse: [ 0 ]) ]