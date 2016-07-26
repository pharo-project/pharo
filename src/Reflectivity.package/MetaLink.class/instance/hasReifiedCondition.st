testing
hasReifiedCondition
	self hasCondition ifFalse: [ ^false ].
	^self condition isKindOf: RFCondition