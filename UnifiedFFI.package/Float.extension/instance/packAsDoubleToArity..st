*UnifiedFFI
packAsDoubleToArity: arity 
	| rolledPointer |
		
	rolledPointer := self.
	1 to: arity do: [ :index | rolledPointer := rolledPointer pointerDouble ].
	^ rolledPointer	