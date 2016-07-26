converting
as31BitSmallInt
	"This is only for 31 bit numbers.  Keep my 31 bits the same, but put them in a small int.  The small int will be negative since my 31st bit is 1.  We know my 31st bit is 1 because otherwise I would already be a positive small int."

	self highBit = 31 ifFalse: [self error: 'more than 31 bits can not fit in a SmallInteger'].

	^ self - 16r80000000