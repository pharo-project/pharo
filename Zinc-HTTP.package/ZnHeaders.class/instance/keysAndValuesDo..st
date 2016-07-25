enumerating
keysAndValuesDo: block
	"Execute a two argument (key, value) block for each header.
	Multi-valued headers are handled transparently."

	self headersDo: block