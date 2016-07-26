tests
testInvalid
	"empty, impossible ranges"
	self assert: (1 to: 0) = #().
	self assert: (1 to: -1) = #().
	self assert: (-1 to: -2) = #().
	self assert: (1 to: 5 by: -1) = #().
	
	"always contains only start value."
	self assert: (1 to: 1) = #(1).
	self assert: (1 to: 5 by: 10) = #(1).
	self assert: (1 to: 0 by: -2) = #(1).
