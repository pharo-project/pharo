as yet unclassified
hasAnyNonIncomingConflicts
	"Answer whether there are any conflicts not marked as incoming."

	^self allConflicts anySatisfy: [:conflict |
		conflict isResolved not or: [conflict localChosen]]