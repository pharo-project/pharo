*Athens-Morphic
createLinesPathFrom: borderVertices on: anAthensCanvas
	^ anAthensCanvas
		createPath: [ :builder | 
			builder absolute.
			borderVertices
				overlappingPairsDo: [ :start :end | 
					builder moveTo: start.
					builder lineTo: end ] ]