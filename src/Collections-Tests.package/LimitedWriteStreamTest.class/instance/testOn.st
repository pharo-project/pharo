tests
testOn
	| stream |
	super testOn.
	stream := self newStream.
	self
		assert: stream limit = self streamClass defaultLimit;
		assert: stream limitBlock isNil