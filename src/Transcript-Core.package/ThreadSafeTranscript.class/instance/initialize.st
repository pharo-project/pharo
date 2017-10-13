initialization
initialize
 
	super initialize.
	accessSemaphore := Mutex new.
	stream := String new writeStream.
	deferredClear := false.
	deferredEndEntry := false.
	stepContents := ''.

	