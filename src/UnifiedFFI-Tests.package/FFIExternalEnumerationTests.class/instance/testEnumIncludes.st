tests
testEnumIncludes
	self assert: (self enumClass includes: #DDD).
	self deny: (self enumClass includes: #EEE)