operations
copyTo: aReference
	self isDirectory
		ifTrue: [ aReference ensureCreateDirectory ]
		ifFalse: [ filesystem copy: path toReference: aReference ]