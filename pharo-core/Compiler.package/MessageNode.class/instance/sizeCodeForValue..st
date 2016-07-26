code generation
sizeCodeForValue: encoder
	| total |
	special > 0 
		ifTrue: 
			[encoder noteOptimizedSelector: originalSelector.
			^self perform: (MacroSizers at: special) with: encoder with: true].
	receiver == NodeSuper
		ifTrue: [selector := selector copy "only necess for splOops"].
	total := selector sizeCode: encoder args: arguments size super: receiver == NodeSuper.
	receiver == nil 
		ifFalse: [total := total + (receiver sizeCodeForValue: encoder)].
	sizes := arguments collect: 
					[:arg | | argSize | 
					argSize := arg sizeCodeForValue: encoder.
					total := total + argSize.
					argSize].
	^total