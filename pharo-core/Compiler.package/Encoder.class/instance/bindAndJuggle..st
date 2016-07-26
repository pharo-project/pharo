temps
bindAndJuggle: name

	| node nodes first thisCode |
	node := self reallyBind: name.

	"Declared temps must precede block temps for decompiler and debugger to work right"
	nodes := self tempNodes.
	(first := nodes findFirst: [:n | n scope > 0]) > 0 ifTrue:
		[node == nodes last ifFalse: [self error: 'logic error'].
		thisCode := (nodes at: first) code.
		first to: nodes size - 1 do:
			[:i | (nodes at: i) key: (nodes at: i) key
							code: (nodes at: i+1) code].
		nodes last key: nodes last key code: thisCode].
	
	^ node