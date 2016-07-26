tests - accessing
testAtPut2
	"self debug: #testAtPut2"
	| array |
	array := RunArray new: 5 withAll: 2.
	
	self should: [array at: 0 put: 5] raise: Error.
	self should: [array at: 6 put: 5] raise: Error.
	self should: [array at: $b put: 5] raise: Error.