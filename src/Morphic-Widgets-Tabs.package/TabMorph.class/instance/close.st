protocol
close

	closeable ifFalse: [ ^ self ].
	self okToClose ifFalse: [ ^self ].
	self delete