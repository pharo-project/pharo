instructions
storeInstVar: index 
	forceLongForm 
		ifTrue: [ encoder genStoreInstVarLong: index - 1 ]
		ifFalse: [ encoder genStoreInstVar: index - 1 ]