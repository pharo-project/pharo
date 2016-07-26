finalization
autoClose
	"Private. Answer true if the file should be automatically closed when 
	this object is finalized."

	^ autoClose
		ifNil: [autoClose := true]