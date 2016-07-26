icons-actions
mouseEnterIcon

	closeable ifFalse: [ ^ self ].
	fadeOut ifNotNil: [ fadeOut terminate ].
	closeIcon image: self class closeOverIcon