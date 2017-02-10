accessing
referencedGlobalNames

	| firstEntry priorReference |
	firstEntry := self log firstEntryIfAbsent: [ ^ self referencedGlobalNamesIfEmptyLog ].
	priorReference := firstEntry tagAt: EpLog priorReferenceKey.
	priorReference isNull ifTrue: [ ^ #() ].
	
	"Could happen when monitor's ombu file is removed externally"
	priorReference globalName = self globalName ifTrue: [ ^ #() ].

	^ { priorReference globalName }