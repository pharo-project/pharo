code generation
sizeCodeExceptLast: encoder
	| codeSize |
	codeSize := 0.
	1 to: statements size - 1 do: 
		[:i | | statement |
		 statement := statements at: i.
		 codeSize := codeSize + (statement sizeCodeForEffect: encoder)].
	^codeSize