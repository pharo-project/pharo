accumulator ops
+= aThirtTwoBitRegister
	"Replace my contents with the sum of the given register and my current contents."

	| lowSum |
	lowSum := low + aThirtTwoBitRegister low.
	hi := (hi + aThirtTwoBitRegister hi + (lowSum bitShift: -16)) bitAnd: 16rFFFF.
	low := lowSum bitAnd: 16rFFFF.
