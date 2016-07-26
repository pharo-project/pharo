*Athens-Morphic
createClosedPolygonPathFrom: aPointsArray on: anAthensCanvas
	^ anAthensCanvas
		createPath: [ :builder | 
			aPointsArray ifEmpty: [ builder ].
			builder absolute.
			builder moveTo: aPointsArray first.
			aPointsArray allButFirstDo: [ :p | builder lineTo: p ].
			builder close.
			builder ]