testing
hasFilterBlock
	^ self parameters at: 'filterBlock' ifPresent: [ true ] ifAbsent: [ false ]