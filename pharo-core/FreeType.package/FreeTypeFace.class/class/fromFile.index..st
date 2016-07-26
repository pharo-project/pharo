instance creation
fromFile: aFileName index: anInteger
	"share alike instances"
	| file |
	file := aFileName asFileReference.
	self rememberFontDir: file parent fullName .
	
	self allInstancesDo: [:inst |
		(inst filename = aFileName and: [inst index = anInteger])
			ifTrue: [^inst "validate"]].
		
	^ (self basicNew)
		filename: aFileName;
		index: anInteger;
		yourself