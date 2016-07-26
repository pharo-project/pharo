initialize
flush
	"Force all pending primitives onscreen"
	engine ifNotNil:[engine flush].