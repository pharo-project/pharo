tests - bitLogic
testTwoComplementBitLogicWithCarry
	"This is non regression test for http://bugs.squeak.org/view.php?id=6874"
	
	"By property of two complement, following operation is:
	...111110000 this is -16
	...111101111 this is -16-1
	...111100000 this is -32, the result of bitAnd: on two complement
	
	This test used to fail with n=31 39 47.... because of bug 6874"
	
	self assert: ((2 to: 80) allSatisfy: [:n | ((2 raisedTo: n) negated bitAnd: (2 raisedTo: n) negated - 1) = (2 raisedTo: n + 1) negated]).