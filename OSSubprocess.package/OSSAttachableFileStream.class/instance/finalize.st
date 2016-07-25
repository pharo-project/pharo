finalization
finalize
	"If #autoClose is true, then we try to close the stream upon finalization - GC "
	
	self autoClose
		ifTrue: [[self primCloseNoError: fileID] on: Error do: []]