testing
isFilterable
	^ self parameters at: 'isFilterable' ifAbsent: [ false ]