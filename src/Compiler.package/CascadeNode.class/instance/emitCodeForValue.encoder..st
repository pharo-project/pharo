code generation
emitCodeForValue: stack encoder: encoder
	receiver emitCodeForValue: stack encoder: encoder.
	1 to: messages size - 1 do: 
		[:i | 
		encoder genDup.
		stack push: 1.
		(messages at: i) emitCodeForValue: stack encoder: encoder.
		encoder genPop.
		stack pop: 1].
	messages last emitCodeForValue: stack encoder: encoder