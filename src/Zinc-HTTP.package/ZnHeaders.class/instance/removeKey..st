accessing
removeKey: headerName	
	^ self headers removeKey: (self normalizeHeaderKey: headerName)