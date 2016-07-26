arithmetic
\\ aNumber 
	"modulo. Remainder defined in terms of //. Answer a Number with the 
	same sign as aNumber. e.g. 9\\4 = 1, -9\\4 = 3, 9\\-4 = -3, 0.9\\0.4 = 0.1."

	^self - (self // aNumber * aNumber)