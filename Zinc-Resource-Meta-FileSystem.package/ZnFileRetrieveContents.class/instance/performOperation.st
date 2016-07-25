operations
performOperation
	"Download and return the FILE resource that url refers to.
	This will typically return a String or a ByteArray (see AbstractFileReference>>#contents).
	If something goes wrong, an exception will be signalled."
	
	^ self url asFileReference contents