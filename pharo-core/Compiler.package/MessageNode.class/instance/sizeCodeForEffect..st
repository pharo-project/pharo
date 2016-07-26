code generation
sizeCodeForEffect: encoder

	special > 0 
		ifTrue:
			[encoder noteOptimizedSelector: originalSelector.
			^self perform: (MacroSizers at: special) with: encoder with: false].
	^super sizeCodeForEffect: encoder