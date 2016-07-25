initialize-release
close
	"Close the stream, flush if necessary"

	[ self flush ] ensure: [ 
		socket ifNotNil: [ 
			self socketClose.
			socket := nil ].
		readBuffer close.
		writeBuffer close ]