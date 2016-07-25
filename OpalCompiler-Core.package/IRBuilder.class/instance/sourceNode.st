mapping
sourceNode
	^ sourceMapNodes
		ifEmpty: [nil]
		ifNotEmpty: [sourceMapNodes last]