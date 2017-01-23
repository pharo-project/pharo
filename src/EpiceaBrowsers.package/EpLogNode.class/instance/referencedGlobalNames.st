accessing
referencedGlobalNames

	| firstEntry priorReference |
	firstEntry := self log firstEntryIfAbsent: [ ^ self referencedGlobalNamesIfEmptyLog ].
	priorReference := firstEntry tagAt: EpLog priorReferenceKey.
	priorReference isNull ifTrue: [ ^ #() ].

	^ { priorReference globalName }