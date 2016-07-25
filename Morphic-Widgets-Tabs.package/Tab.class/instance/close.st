protocol
close

	closeable ifFalse: [ ^ self ].
	self delete