accessing
at: headerName put: value
	"Store value under headerName, replace existing entries"
	
	^ self headers at: (self normalizeHeaderKey: headerName) put: value