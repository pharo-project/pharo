icons-actions
mouseLeaveIcon

	closeable ifFalse: [ ^ self ].
	fadeOut ifNotNil: [ fadeOut terminate ].
	fadeOut := [ 
		closeIcon image: self class closeHalfOverIcon.
		50 milliSeconds asDelay wait.
		closeIcon image: self class closeIcon ] fork