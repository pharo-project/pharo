private - layout
layoutChanged
	"Don't pass to owner, since the receiver doesn't care! Improves frame rate."
	
	fullBounds := nil.
	self layoutPolicy ifNotNil:[:l | l flushLayoutCache].