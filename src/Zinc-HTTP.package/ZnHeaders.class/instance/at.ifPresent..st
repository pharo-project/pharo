accessing
at: headerName ifPresent: block
	"Like #at: but executes block when the headerName is found"

	^ self headers at: (self normalizeHeaderKey: headerName) ifPresent: block