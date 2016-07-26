testing
testReadingNext
	| data chunked plain |
	data := String withAll: ($a to: $z), ($A to: $Z).
	chunked := String streamContents: [ :stream |
		self nextPutAll: data on: stream chunked: 16 ].
	plain := (ZnChunkedReadStream on: chunked readStream) next: 26 + 26.
	self assert: plain equals: data