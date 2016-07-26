tests - NaN behavior
testNaN2
	"Two NaN values are always considered to be different.
	On an little-endian machine (32 bit Intel), Float nan is 16rFFF80000 16r00000000.
	On a big-endian machine (PowerPC), Float nan is 16r7FF80000 16r00000000. Changing
	the bit pattern of the first word of a NaN produces another value that is still
	considered equal to NaN. This test should work on both little endian and big
	endian machines. However, it is not guaranteed to work on future 64 bit versions
	of Squeak, for which Float may have different internal representations."

	"FloatTest new testNaN2"

	| nan1 nan2 |
	nan1 := Float nan copy.
	nan2 := Float nan copy.

	"test two instances of NaN with the same bit pattern"
	self deny: nan1 = nan2.
	self deny: nan1 == nan2.
	self deny: nan1 = nan1.
	self assert: nan1 == nan1.

	"change the bit pattern of nan1"
	self assert: nan1 size = 2.
	self assert: (nan1 at: 2) = 0.
	nan1 at: 1 put: (nan1 at: 1) + 999.
	self assert: nan1 isNaN.
	self assert: nan2 isNaN.
	self deny: (nan1 at: 1) = (nan2 at: 1).

	"test two instances of NaN with different bit patterns"
	self deny: nan1 = nan2.
	self deny: nan1 == nan2.
	self deny: nan1 = nan1.
	self assert: nan1 == nan1
