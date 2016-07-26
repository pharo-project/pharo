geometry etoy
referencePosition 
	"Return the current reference position of the receiver"
	^ self valueOfProperty: #referencePosition ifAbsent: [ self center ]
