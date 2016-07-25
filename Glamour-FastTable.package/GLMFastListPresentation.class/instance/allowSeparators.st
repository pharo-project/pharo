testing
allowSeparators
	^ self parameters at: 'withSeparators' ifAbsent: [ false ]