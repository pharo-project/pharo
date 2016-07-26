*Athens-Morphic
adjustSegmentPointsForArrows
	" In the list of vertices, adjust first and last entry if this polygon uses arrows."

	| verts |
	(self isClosed or: [ self hasArrows not ])
		ifTrue: [ ^ self vertices ].
	verts := self vertices copy.
	(arrows == #back or: [ arrows == #both ])
		ifTrue: [ 
			| arrow |
			arrow := self arrowBoundsAt: verts first from: verts second.
			arrow size = 4
				ifTrue: [ verts at: 1 put: arrow third ]
				ifFalse: [ verts at: 1 put: (arrow copyFrom: 2 to: 3) average ] ].
	(arrows == #forward or: [ arrows == #both ])
		ifTrue: [ 
			| arrow |
			arrow := self arrowBoundsAt: verts last from: verts nextToLast.
			arrow size = 4
				ifTrue: [ verts at: verts size put: arrow third ]
				ifFalse: [ verts at: verts size put: (arrow copyFrom: 2 to: 3) average ] ].
	^ verts