slots
slotNamed: aName ifFound: foundBlock ifNone: exceptionBlock
	"Tratis have no slots"
	^self error: 'no slot found'.