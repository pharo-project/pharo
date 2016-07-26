visiting
visitPushClosureCopy: closure

	stream nextPutAll: 'pushClosureCopyCopiedValues: '.
	stream nextPutAll: closure copiedValues printString .
	stream nextPutAll: ' args: '.
	stream nextPutAll: closure arguments printString.
