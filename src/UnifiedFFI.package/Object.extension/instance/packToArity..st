*UnifiedFFI
packToArity: arity
	"This will answer a pointer to this object. 
	 It is like doing ==var ptr=&aVariable== in C (up to arity). 
	
	 In general, arity>1 will not work because then object has to be copied to 
	 heap, but my overrides will handle this case"
	| rolledPointer |
		
	rolledPointer := self.
	1 to: arity do: [ :index | rolledPointer := rolledPointer pointer ].
	^ rolledPointer