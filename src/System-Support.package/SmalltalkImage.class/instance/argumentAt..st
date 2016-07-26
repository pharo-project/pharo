to clean later
argumentAt: i
	"Answer the i-th argument of the command line, or nil if not so many argument."
	
	^self vm getSystemAttribute: 2 + i