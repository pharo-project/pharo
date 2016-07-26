private-packing
nextByte
	bufStream atEnd
		ifTrue:
			[self atEnd ifTrue: [^nil].
			self fillBuffer].
	^bufStream next