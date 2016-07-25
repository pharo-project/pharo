accessing
stream
	"The file stream I am writing to.
	Lazy initialized so as not to create the file when it is not used."
	
	^ stream ifNil: [ self initializeStream ]