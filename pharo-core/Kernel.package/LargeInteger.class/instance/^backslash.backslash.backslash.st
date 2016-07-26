arithmetic
\\\ anInteger 
	"a faster modulo method for use in DSA. Be careful if you try to use this elsewhere"

	^(self digitDiv: anInteger neg: false) second