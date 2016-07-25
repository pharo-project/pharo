accessing
isAutoFit
	^ self valueOfProperty: #autoFitContents ifAbsent: [true]
