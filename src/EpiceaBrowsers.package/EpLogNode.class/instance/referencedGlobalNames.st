accessing
referencedGlobalNames

	| firstEntry priorReference |
	firstEntry := self log firstEntryIfAbsent: [ ^ self referencedGlobalNamesIfEmptyLog ].
	priorReference := firstEntry tags at: EpLog priorReferenceKey ifAbsent: [ ^ #() ].
	priorReference isNull ifTrue: [ ^ #() ].
	
	"Could happen when monitor's ombu file is removed externally"
	priorReference globalName = self globalName ifTrue: [ ^ #() ].

	^ { priorReference globalName }