optimizing
optimize
	self removeEmptyStart.
	self compilationContext optionOptimizeIR ifFalse: [^self].
	self absorbJumpsToSingleInstrs.
	
	"This last two optimizations works but the problem 
	when you absorb conditional jumps 
	is that and: / or: chained generate explicitely 
	constant conditional jumps to tip the JIt and the 
	decompiler that they are and: or: and not conditions/loops
	
	self absorbConstantConditionalJumps.
	self absorbJumpsToSingleInstrs"