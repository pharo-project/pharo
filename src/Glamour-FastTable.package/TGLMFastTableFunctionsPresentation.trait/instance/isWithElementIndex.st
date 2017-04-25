accessing
isWithElementIndex
	^ self parameters at: 'withRowIndex' ifAbsent: [ false ]