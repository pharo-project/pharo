accumulator ops
bitXor: aThirtTwoBitRegister
	"Replace my contents with the bitwise exclusive OR of the given register and my current contents."

	hi := hi bitXor: aThirtTwoBitRegister hi.
	low := low bitXor: aThirtTwoBitRegister low.
