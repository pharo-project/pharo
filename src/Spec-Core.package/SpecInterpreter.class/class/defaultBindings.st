private
defaultBindings
	
	^ (CurrentSpecDefaultBindings value ifNil: [ self environment at: #MorphicAdapterBindings]) new
	