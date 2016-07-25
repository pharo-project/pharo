commands
version
	self stdout 
		nextPutAll: 'Image: '; print: SystemVersion current; cr;
		nextPutAll: 'VM:    '; nextPutAll: Smalltalk vm version; cr.
	self quit.