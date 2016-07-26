accumulator ops
bitAnd: aThirtTwoBitRegister
	"Replace my contents with the bitwise AND of the given register and my current contents."

	hi := hi bitAnd: aThirtTwoBitRegister hi.
	low := low bitAnd: aThirtTwoBitRegister low.
