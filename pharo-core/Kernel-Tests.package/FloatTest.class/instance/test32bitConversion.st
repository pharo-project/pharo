tests - IEEE 754
test32bitConversion
	"Except for NaN, we can convert a 32bits float to a 64bits float exactly.
	Thus we can convert the 64bits float to the original 32bits float pattern."
	
	#(16r0 "zero"
	 16r80000000 "negative zero"
	 16r1 "min unormalized"
	 16r12345 "a unnormalized"
	 16r801FEDCB "a negative unnormalized"
	 16r7FFFFF "largest unnormalized"
	 16r800000 "smallest normalized"
	 16r468ACDEF "a normalized float"
	 16rCABD1234 "a negative normalized float"
	 16r7F7FFFFF "largest finite float"
	 16r7F800000 "positive infinity"
	 16rFF800000 "negative infinity"
	)
	  do: [:originalWord | self assert: (Float fromIEEE32Bit: originalWord) asIEEE32BitWord = originalWord]