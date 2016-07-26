tests
testSmallIntegers
	| integers |
	integers := STON listClass withAll: (-10 to: 10).
	self serializeAndMaterialize: integers