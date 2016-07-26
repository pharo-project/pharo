private
basicOpen
	"get the raw stream description from the filesystem's store"
	^ self fileSystem 
		openStreamDescription: reference path
		writable: writable