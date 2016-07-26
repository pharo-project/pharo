tests - testing
testIfNotNilIfNil

	self should: [ nil ifNotNil: [self error] ifNil: [self halt] ] raise: Halt.


