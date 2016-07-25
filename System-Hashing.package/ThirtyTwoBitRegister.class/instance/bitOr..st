accumulator ops
bitOr: aThirtTwoBitRegister
	"Replace my contents with the bitwise OR of the given register and my current contents."

	hi := hi bitOr: aThirtTwoBitRegister hi.
	low := low bitOr: aThirtTwoBitRegister low.
