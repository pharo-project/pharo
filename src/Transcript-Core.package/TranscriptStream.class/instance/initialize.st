initialization
initialize
 
	super initialize.
	mutex := Mutex new.
	stream := String new writeStream.
	outputs := Set new.
	updateStrategy := nil.