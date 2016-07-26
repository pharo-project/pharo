private
createClosedPolygonPathFrom: aPointsArray
	^ self canvas
		createPath: [ :builder | 
			aPointsArray ifEmpty: [ builder ].
			builder absolute.
			builder moveTo: aPointsArray first.
			aPointsArray allButFirstDo: [ :p | builder lineTo: p ].
			builder close.
			builder ]