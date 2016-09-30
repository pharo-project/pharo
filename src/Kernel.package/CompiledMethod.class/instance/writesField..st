scanning
writesField: varIndex
	"Answer whether the receiver stores into the instance variable indexed by the argument."
	self isQuick ifTrue: [^false].
	^ super writesField: varIndex