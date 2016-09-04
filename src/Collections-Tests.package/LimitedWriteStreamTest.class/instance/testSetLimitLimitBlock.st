tests
testSetLimitLimitBlock
	| stream overLimit |
	stream := self newStream.
	overLimit := false.
	self assert:
		(stream
			setLimit: self stringSize
			limitBlock: [ overLimit := true ]) == stream.
	stream nextPutAll: self string.
	self deny: overLimit.

	self stringSize - 1 to: 0 by: -1 do: [ :i |
		overLimit := false.
		"ensure the new block argument is used when the new limit is less than
		the position by setting it to nil first"
		self assert:
			(stream
				setLimit: stream position
				limitBlock: nil) == stream.
		self deny: overLimit.
		self assert:
			(stream
				setLimit: i
				limitBlock: [ overLimit := true ]) == stream.
		self assert: overLimit].