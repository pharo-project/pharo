file paths
platformVMRelativeDirectories
	
	| directory |
	directory :=  Smalltalk vm path asFileReference / 'Fonts'.
	directory exists
		ifTrue: [ ^ { directory } ].
	^ #()