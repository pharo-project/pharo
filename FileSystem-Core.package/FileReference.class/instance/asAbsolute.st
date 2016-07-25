converting
asAbsolute
	"Return the receiver as an absolute file reference."
	
	^ self isAbsolute
		ifTrue: [ self ]
		ifFalse: [ filesystem referenceTo: (filesystem resolve: path) ]