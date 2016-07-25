testing
includesKey: headerName
	^ self isEmpty not and: [ self headers includesKey: (self normalizeHeaderKey: headerName) ]
	