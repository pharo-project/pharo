event handling
mouseUp: evt
	self scrollPivot: nil.
	(self boundsInWorld containsPoint: self currentHand position)
		ifFalse: [ self hideOverEditableTextCursor ].
	mouseDownPoint := nil