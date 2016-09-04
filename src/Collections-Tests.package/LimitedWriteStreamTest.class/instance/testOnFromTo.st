tests
testOnFromTo
	| stream |
	super testOnFromTo.
	stream := self streamClass
		on: self string , self string
		from: self stringSize + 1
		to: self stringSize * 2.
	self
		assert: stream limit = self streamClass defaultLimit;
		assert: stream limitBlock isNil