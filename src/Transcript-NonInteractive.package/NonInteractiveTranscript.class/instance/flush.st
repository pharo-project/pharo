streaming
flush
	"Show the currently buffered output"

	self critical: [ self stream flush ]