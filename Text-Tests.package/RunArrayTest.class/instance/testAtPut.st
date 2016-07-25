tests - accessing
testAtPut
	"self debug: #testAtPut"
	| array |
	array := RunArray new: 5 withAll: 2.
	
	array at: 3 put: 5.
	self assert: array asArray = #(2 2 5 2 2).
	
	array at: 1 put: 1.
	self assert: array asArray = #(1 2 5 2 2).