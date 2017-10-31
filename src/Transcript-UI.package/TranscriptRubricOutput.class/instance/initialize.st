initialization
initialize

	super initialize.
	
	cleared := false.
	connections := WeakSet new.
	stream := String new writeStream.
	