utilities
universalTestFor: aChange
	self equalityTestFor: aChange.
	self stringTestFor: aChange.
	(aChange isKindOf: RBRefactoryClassChange)
		ifTrue: [ self undoTestFor: aChange ]