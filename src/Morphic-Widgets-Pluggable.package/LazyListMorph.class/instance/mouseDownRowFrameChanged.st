row management
mouseDownRowFrameChanged
	"Invalidate frame of the current mouse down row if any."
	
	|frame row|
	row := self mouseDownRow ifNil: [ ^self ].
	frame := self selectionFrameForRow: row.
	self invalidRect: frame