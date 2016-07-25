operations
copyTo: aReference
	self isDirectory
		ifTrue: [ aReference ensureCreateDirectory ]
		ifFalse: [ 
			filesystem = aReference fileSystem
				ifTrue: [ filesystem copy: path to: aReference resolve path ]
				ifFalse: [ filesystem copy: path toReference: aReference ] ]