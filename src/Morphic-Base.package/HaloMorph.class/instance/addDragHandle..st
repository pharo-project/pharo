handles
addDragHandle: haloSpec
	(self addHandle: haloSpec on: #mouseDown send: #startDrag:with: to: self)
		on: #mouseMove send: #doDrag:with: to: self


