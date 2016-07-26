public - string reading
sourcedDataAt: sourcePointer

	^ self 
		readStreamAt: sourcePointer 
		ifPresent: [ :file | 
			| position |
			position := self filePositionFromSourcePointer: sourcePointer.
			self getPreambleFrom: file at: (0 max: position - 3) ]
		ifAbsent: [ '' ]