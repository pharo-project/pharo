private-packing
flushBuffer
	bufStream isEmpty ifTrue: [ ^ self ].
	self nextPut: bufStream size.
	self nextPutAll: bufStream contents.
	bufStream := (ByteArray new: 256) writeStream.