accessing
referencedGlobalNames

	| firstEntry priorReference |
	firstEntry := self log firstEntryIfAbsent: [ ^ #() ].
	priorReference := firstEntry tagAt: EpLog priorReferenceKey.
	priorReference isNull ifTrue: [ ^ #() ].

	^ { priorReference globalName }