system startup
shutDown  "Form shutDown"
	"Compress all instances in the system.  Will decompress on demand..."
	Form allInstancesDo: [:f | f hibernate].
	ColorForm allInstancesDo: [:f | f hibernate].