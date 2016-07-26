*Athens-Morphic
asAthensLinePathOn: anAthensCanvas
	| verts |
	verts := self adjustSegmentPointsForArrows.
	^ anAthensCanvas
		createPath: [ :builder | 
			verts notEmpty
				ifTrue: [ 
					builder absolute.
					builder moveTo: verts first.
					verts allButFirstDo: [ :p | builder lineTo: p ] ].
			self isClosed
				ifTrue: [ builder close ].
			builder ]