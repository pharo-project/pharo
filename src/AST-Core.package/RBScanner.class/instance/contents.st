accessing
contents
	| contentsStream |
	contentsStream := (Array new: 50) writeStream.
	[ self atEnd ]
		whileFalse: [ contentsStream nextPut: self next ].
	^ contentsStream contents