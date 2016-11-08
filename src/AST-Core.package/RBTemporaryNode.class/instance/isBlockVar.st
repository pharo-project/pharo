testing
isBlockVar
	"true if a variable node is defined by a node whos parent is a block"

	^  (self whoDefines: self name) parent isBlock