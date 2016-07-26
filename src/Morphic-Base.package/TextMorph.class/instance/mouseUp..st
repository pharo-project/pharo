accessing
mouseUp: evt
	self handleInteraction: [editor mouseUp: evt].
	(self boundsInWorld containsPoint: self currentHand position)
		ifFalse: [self hideOverEditableTextCursor]