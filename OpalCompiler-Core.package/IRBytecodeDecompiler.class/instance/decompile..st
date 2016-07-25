public access
decompile: aCompiledMethod
	instructionStream := InstructionStream on: aCompiledMethod.
	irBuilder := IRReconstructor new.
	scopeStack := Stack new.
	self pushScope: #() numArgs: aCompiledMethod numArgs.
	irBuilder irPrimitive: aCompiledMethod irPrimitive.
	
	aCompiledMethod hasProperties ifTrue: [irBuilder properties: aCompiledMethod properties copy].
	irBuilder numArgs: aCompiledMethod numArgs.
	irBuilder addTemps: self scope args.
	aCompiledMethod isQuick
		ifTrue: [self quickMethod]
		ifFalse: [self interpret].
	self popScope.
	"just add all literals of the compiledMethod as additional literals.
	 duplicates will be filtered out, but we keep the optimized ones"
	irBuilder additionalLiterals: (aCompiledMethod literals allButLast: 2).
	"when compiling methods of context classes, force long form for iVar access by getting the correct context"
	irBuilder compilationContext: aCompiledMethod methodClass compiler compilationContext.
	^ irBuilder ir.