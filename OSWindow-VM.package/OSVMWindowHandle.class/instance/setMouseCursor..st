accessing
setMouseCursor: cursorWithMask

	Cursor currentCursor == cursorWithMask ifFalse: [  
		Cursor currentCursor: cursorWithMask ].