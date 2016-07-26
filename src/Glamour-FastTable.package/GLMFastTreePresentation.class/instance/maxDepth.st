accessing
maxDepth
	^ self parameters at: 'maxDepth' ifAbsent: [ nil ]