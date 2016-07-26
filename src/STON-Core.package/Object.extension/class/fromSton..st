*ston-core
fromSton: stonReader
	"Create a new instance and delegate decoding to instance side.
	Override only when new instance should be created directly (see implementors). "
	
	^ self new
		fromSton: stonReader;
		yourself