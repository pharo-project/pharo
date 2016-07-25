*Slot
temporaryVariableNamed: aName
	(self hasTemporaryVariableNamed: aName) ifFalse: [ ^nil ].
	^TemporaryVariable 
		name: aName 
		block: self 
