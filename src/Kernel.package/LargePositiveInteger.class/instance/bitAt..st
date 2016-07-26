bit manipulation
bitAt: anInteger
	"Optimize super algorithm to avoid long bit operations.
	Instead work on digits which are known to be SmallInteger and fast.
	Note that this algorithm does not work for negative integers."
	
	| digitIndex bitIndex |
	digitIndex := anInteger - 1 // 8 + 1.
	digitIndex > self digitLength ifTrue: [^0].
	bitIndex := anInteger - 1 \\ 8 + 1.
	^(self digitAt: digitIndex) bitAt: bitIndex