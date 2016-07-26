tests - accessing
testFirst
	"self debug: #testFirst"
	| array |
	array := RunArray new: 5 withAll: 2.
	self assert: array first = 2.
	
	array := #($a $b $c $d) as: RunArray.
	self assert: array first = $a.