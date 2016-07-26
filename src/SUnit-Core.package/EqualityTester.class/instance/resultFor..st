operation
resultFor: runs 
	"Test that equality is the same over runs and answer the result"
	1
		to: runs
		do: [:i | self prototype = self prototype
				ifFalse: [^ false]]. 
	^ true