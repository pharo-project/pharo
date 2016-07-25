accessing
packageSnapshot
	^ packageSnapshot ifNil: [packageSnapshot := version package snapshot]