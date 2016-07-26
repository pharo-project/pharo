*Collections-arithmetic
preMultiplyByArray: a
	"Answer a+*self where a is an Array.  Arrays are always understood as column vectors,
	 so an n element Array is an n*1 Array.  This multiplication is legal iff self size = 1."

	self size = 1 ifFalse: [self error: 'dimensions do not conform'].
	^a * self first