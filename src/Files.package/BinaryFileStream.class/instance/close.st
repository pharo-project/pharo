opening-closing
close
	self closed
		ifTrue: [ ^ self ].
	File primClose: handle.
	self unregister.
	handle := nil