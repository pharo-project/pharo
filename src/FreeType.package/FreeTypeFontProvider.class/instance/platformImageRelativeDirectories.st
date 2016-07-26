file paths
platformImageRelativeDirectories
	
	| directory |
	directory :=  Smalltalk imagePath asFileReference / 'Fonts'.
	directory exists
		ifTrue: [ ^ { directory } ].
	^ #()