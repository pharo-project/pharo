accessing
at: headerName ifAbsent: block
	"Like #at: but executes block when the headerName is not found"
	
	self isEmpty ifTrue: [ ^ block value ].
	^ self headers at: (self normalizeHeaderKey: headerName) ifAbsent: block