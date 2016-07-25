event handling
mouseDown: evt
	(self bounds containsPoint: evt cursorPoint)
		ifTrue: [	
			evt wasHandled: true.
			^ self 
				selectIndexAtPoint: evt cursorPoint;
				insertSelected ].
			
	super mouseDown: evt.
	evt wasHandled: false.
	self flag: #fixme "ugly hack".
	controller editor morph owner owner
		takeKeyboardFocus;
		handleMouseDown: evt.
	self close.