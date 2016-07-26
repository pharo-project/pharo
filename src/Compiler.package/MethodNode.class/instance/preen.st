converting
preen
	"Preen for pretty-printing and/or decompilation.
	 i.e. post-process to cover up for inadequacies in both algorithms.
	 Currently one case, hiding the assignment to the arg of an inlined block arg to ifNotNil:,
		(var := expr) ifNil: [...] ifNotNil: [...]    =>    expr ifNil: [...] ifNotNil: [:var| ...]."

	self preenLocalIfNotNilArg