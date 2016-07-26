accessing
at: headerName
	"Return the value stored under headerName,
	a String for single-valued headers, an Array of Strings for multi-valued headers"
	
	^ self headers at: (self normalizeHeaderKey: headerName)