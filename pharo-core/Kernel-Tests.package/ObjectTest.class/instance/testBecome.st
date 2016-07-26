tests
testBecome
	"self debug: #testBecome"
	"this test should that all the variables pointing to an object are pointing now to another one, and all
      object pointing to the other are pointing to the object"

	| pt1 pt2 pt3 |
	pt1 := 0@0.
	pt2 := pt1.
	pt3 := 100@100.

	pt1 become: pt3.
	self assert: pt2 = (100@100).
	self assert: pt3 = (0@0).
	self assert: pt1 = (100@100).