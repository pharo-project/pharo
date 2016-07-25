accessing
socketStreamClass
	^ socketStreamClass ifNil: [ socketStreamClass := SocketStream ]