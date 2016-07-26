accumulator ops
bitInvert
	"Replace my contents with the bitwise inverse my current contents."

	hi := hi bitXor: 16rFFFF.
	low := low bitXor: 16rFFFF.
