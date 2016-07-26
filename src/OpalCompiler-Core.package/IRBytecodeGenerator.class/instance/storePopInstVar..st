instructions
storePopInstVar: index 
	stack pop.
	forceLongForm 
		ifTrue: [ encoder genStorePopInstVarLong: index - 1 ]
		ifFalse: [ encoder genStorePopInstVar: index - 1 ]