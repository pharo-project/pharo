*System-Object Events
hasActionsWithReceiver: anObject

	^self actionMap keys anySatisfy:
		[:eachEventSelector |
			(self actionSequenceForEvent: eachEventSelector)
				anySatisfy: [:anAction | anAction receiver == anObject]]