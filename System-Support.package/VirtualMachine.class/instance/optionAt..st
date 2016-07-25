accessing
optionAt: i
	"Answer the i-th option of the command line, or nil if not so many options."
	
	^self getSystemAttribute: i negated