testing
isBlockVar
	"true if a variable node is defined by a block node"

	^ (self whoDefines: self name) isBlock