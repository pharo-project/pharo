morphic ui
versionSelection: aNumber
	aNumber isZero 
		ifTrue: [ selectedVersion := version := versionInfo := nil ]
		ifFalse: [ 
			selectedVersion := (self versionList at: aNumber) asString.
			version := versionInfo := nil].
	self changed: #versionSelection; changed: #summary; changed: #hasVersion