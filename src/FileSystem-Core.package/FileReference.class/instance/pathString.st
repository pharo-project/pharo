printing
pathString
	"Return the full path name of the receiver."
	
	^ filesystem stringFromPath: (filesystem resolve: path)