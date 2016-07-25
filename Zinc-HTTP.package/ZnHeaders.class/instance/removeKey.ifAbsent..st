accessing
removeKey: headerName ifAbsent: block
	self isEmpty ifTrue: [ ^ block value ].
	^ self headers removeKey: (self normalizeHeaderKey: headerName) ifAbsent: block