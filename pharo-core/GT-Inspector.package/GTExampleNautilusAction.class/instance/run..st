order
run: aCompiledMethod
	aCompiledMethod gtExamples do: [ :example | 
		example run isSuccess ifFalse: [ example debug ] ]