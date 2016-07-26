tests - instance creation
testNewWithAll
	"self debug: #testNewWithAll"
	| array |
	array := RunArray new: 5 withAll: 2.
	self assert: array size = 5.
	self assert: array asArray = #(2 2 2 2 2)