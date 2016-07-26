tests
testBecomeForward
	"self debug: #testBecomeForward"
	"this test should that all the variables pointing to an object are pointing now to another one.
	Not that this inverse is not true. This kind of become is called oneWayBecome in VW"

	| pt1 pt2 pt3 |
	pt1 := 0@0.
	pt2 := pt1.
	pt3 := 100@100.
	pt1 becomeForward: pt3.
	self assert: pt2 = (100@100).
	self assert: pt3 == pt2.
	self assert: pt1 = (100@100)