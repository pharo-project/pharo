miscellaneous
closeLog: logger
	"This is a _private_ method,
	Because it really belongs to logging facility,
	we should delegate to it at some point "
	
	logger ifNotNil: [ logger close ]
