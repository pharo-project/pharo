options
beBinary
	"Configure me to read HTTP responses without interpreting them further,
	their resources/entities will remain bytes."
	
	self reader: [ :stream | ZnResponse readBinaryFrom: stream ]