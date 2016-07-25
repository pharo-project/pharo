tests - accessing
testLast
	"self debug: #testLast"
	| array |
	array := RunArray new: 5 withAll: 2.
	self assert: array last = 2.
	
	array := #($a $b $c $d) as: RunArray.
	self assert: array last = $d.