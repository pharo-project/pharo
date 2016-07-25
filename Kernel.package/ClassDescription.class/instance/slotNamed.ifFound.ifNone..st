slots
slotNamed: aName ifFound: foundBlock ifNone: exceptionBlock
	^self classLayout resolveSlot: aName asSymbol ifFound: foundBlock ifNone: exceptionBlock