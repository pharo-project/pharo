testing
testReadingNextInto
	| data chunked plain |
	data := String withAll: ($a to: $z), ($A to: $Z).
	chunked := String streamContents: [ :stream |
		self nextPutAll: data on: stream chunked: 16 ].
	plain := String new: 26 + 26.
	(ZnChunkedReadStream on: chunked readStream) next: 26 + 26 into: plain.
	self assert: plain equals: data