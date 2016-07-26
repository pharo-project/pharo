class variables
allClassVarNames
	"Answer a Set of the names of the receiver's class variables, including those
	defined in the superclasses of the receiver."

	| aSet |
	self superclass == nil
		ifTrue: 
			[^self classVarNames asSet]  "This is the keys so it is a new Set."
		ifFalse: 
			[aSet := self superclass allClassVarNames.
			aSet addAll: self classVarNames.
			^aSet]