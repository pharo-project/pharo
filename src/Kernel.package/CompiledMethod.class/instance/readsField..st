scanning
readsField: varIndex 
	"Answer whether the receiver loads the instance variable indexed by the argument."
	self isReturnField ifTrue: [^self returnField = (varIndex - 1)].
	^ super readsField: varIndex 