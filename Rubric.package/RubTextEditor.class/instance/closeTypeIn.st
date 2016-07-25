typing support
closeTypeIn
	"See comment in openTypeIn. It is important to call
	closeTypeIn before executing any non-typing key, making a new selection, etc. It is
	called automatically for menu commands."
	self startOfTyping 
		ifNotNil: [self addTypeInUndoRecord.
			self doneTyping]