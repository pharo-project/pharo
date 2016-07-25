tests - compare
testComparison
	
	"test equality when Float conversion loose bits"
	| a b c |
	a := 16r1FFFFFFFFFFFFF1.
	b := 16r1FFFFFFFFFFFFF3.
	c := a asFloat.
	self assert: ((a = c) & (b = c)) ==> (a = b).
	
	"Test equality when Float conversion exact"
	self assert: 16r1FFFFFFFFFFFFF = 16r1FFFFFFFFFFFFF asFloat.
	self assert: 16r1FFFFFFFFFFFFF = 16r1FFFFFFFFFFFFF asFloat asInteger.
	
	"Test inequality when Float conversion loose bits"
	self assert: (((1 bitShift: 54)+1)/(1 bitShift: 54)) > 1.
	self assert: (((1 bitShift: 54)+1)/(1 bitShift: 54)) > 1.0.
	
	self assert: (((1 bitShift: 54)-1)/(1 bitShift: 54)) < 1.
	self assert: (((1 bitShift: 54)-1)/(1 bitShift: 54)) < 1.0.
	
	"Test exact vs inexact arithmetic"
	(1 to: 100) do: [:i |
		i isPowerOfTwo
			ifTrue: [self assert: (1/i) = (1/i) asFloat]
			ifFalse: [self deny: (1/i) = (1/i) asFloat]].
	
	"Test overflow (compare to infinity)"
	a := (11 raisedTo: 400) / 2.
	b := (13 raisedTo: 400) / 2.
	c := a asFloat.
	self assert: ((a = c) & (b = c)) ==> (a = b).
	
	"every integer is smaller than infinity"
	self assert: a < Float infinity.
	self assert: a > Float infinity negated.
	
	"Test underflow"
	self deny: 1 / (11 raisedTo: 400) = 0.
	self deny: 1 / (11 raisedTo: 400) = 0.0.
	
	"Test hash code"
	self assert:
		((Set new: 3) add: 3; add: 3.0; size) =
		((Set new: 4) add: 3; add: 3.0; size).