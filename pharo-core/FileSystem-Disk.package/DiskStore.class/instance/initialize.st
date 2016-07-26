initialization
initialize
	super initialize.
	maxFileNameLength := Smalltalk vm maxFilenameLength ifNil: [ 255 ].