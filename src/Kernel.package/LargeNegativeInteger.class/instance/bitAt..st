bit manipulation
bitAt: anInteger
	"super would not work because we have to pretend we are in two-complement.
	this has to be tricky..."
	
	| digitIndex bitIndex i |
	digitIndex := anInteger - 1 // 8 + 1.
	digitIndex > self digitLength ifTrue: [^1].
	bitIndex := anInteger - 1 \\ 8 + 1.

	i := 1.
	[i = digitIndex
		ifTrue:
			["evaluate two complement (bitInvert + 1) on the digit :
			(if digitIndex > 1, we must still add 1 due to the carry).
			but x bitInvert is -1-x, bitInvert+1 is just x negated..."
			^(self digitAt: digitIndex) negated bitAt: bitIndex].
	(self digitAt: i) = 0]
		whileTrue: [
			"two complement (bitInvert + 1) raises a carry:
			0 bitInvert -> 2r11111111.  2r11111111 + 1 -> 0 with carry...
			Thus we must inquire one digit forward"
			i := i + 1].
	
	"We escaped the while loop, because there is no more carry.
	Do a simple bitInvert without a carry"
	^1 - ((self digitAt: digitIndex) bitAt: bitIndex)