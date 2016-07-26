instance creation
newWith: fileIDsArray readBlocking: readBlocking
	"fileIDsArray is an array of two elements where the first represents a sqFile struct of the reader of the pipe and the second represents the sqFile of the writer of the pipe. 
	If readBlocking is true, then the pipe will lock the reading waiting until there is data. If false, then the streams will not lock and answer immediatly instead.  
	 "
	^ self basicNew 
			initializeWith: fileIDsArray readBlocking: readBlocking
			yourself

