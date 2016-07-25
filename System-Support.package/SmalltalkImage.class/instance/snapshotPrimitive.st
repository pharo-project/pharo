snapshot and quit
snapshotPrimitive
	"Primitive. Write the current state of the object memory on a file in the
	same format as the Smalltalk-80 release. The file can later be resumed,
	returning you to this exact state. Return normally after writing the file.
	Essential. See Object documentation whatIsAPrimitive."
	
	"I will return 
		true if the image is starting or
		false if the image is just resuming"

	<primitive: 97>
	^nil "indicates error writing image file"