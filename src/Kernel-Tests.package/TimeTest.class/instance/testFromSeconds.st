tests
testFromSeconds
	| t |
	t := self timeClass fromSeconds: 14567.
	self
		assert: t = time
