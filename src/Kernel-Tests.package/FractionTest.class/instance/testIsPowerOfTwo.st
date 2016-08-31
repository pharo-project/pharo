tests - conversions
testIsPowerOfTwo
	"Negative fraction"
	self deny: (1 / (2 raisedTo: 80) negated) isPowerOfTwo.
	self deny: (1 negated / (2 raisedTo: 80)) isPowerOfTwo.
	self deny: ((2 raisedTo: 80) negated / 3) isPowerOfTwo.
	self deny: ((2 raisedTo: 80) / 4 negated) isPowerOfTwo.
	"Positive fraction"
	self assert: (1 / 2) isPowerOfTwo.
	self assert: (1 / (2 raisedTo: 80)) isPowerOfTwo.
	self assert: (1 negated / (2 raisedTo: 80) negated) isPowerOfTwo.
	self deny: ((2 raisedTo: 80) / 3) isPowerOfTwo.
	self assert: ((2 raisedTo: 80) / 4) isPowerOfTwo