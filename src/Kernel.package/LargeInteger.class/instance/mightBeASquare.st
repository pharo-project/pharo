mathematical functions
mightBeASquare
	"In base 16, a square number can end only with 0,1,4 or 9 and
	- in case 0, only 0,1,4,9 can precede it,
	- in case 4, only even numbers can precede it.
	See http://en.wikipedia.org/wiki/Square_number
	So, in hex, the last byte must be one of:
		00
		10
		40
		90
		x1
		e4
		x9
	where x is any hex digit and e is any even digit
	Also, the receiver must be an aven power of two."
	| lsb |
	lsb := self digitAt: 1.
	^(lsb = 0 and: [ self lowBit odd ])	"00 (and even power of 2)"
		or: [ lsb = 16r40				"40"
		or: [ (lsb bitAnd: 16r7) = 1		"any|1 or any|9"
		or: [ (lsb bitAnd: 16r1F) = 4		"even|4"
		or: [ (lsb bitAnd: 16r7F) = 16 ]]]]	"10 or 90"