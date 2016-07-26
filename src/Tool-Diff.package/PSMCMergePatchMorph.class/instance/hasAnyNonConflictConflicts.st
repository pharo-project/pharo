as yet unclassified
hasAnyNonConflictConflicts
	"Answer whether there are any conflicts not marked as a conflict."

	^self allConflicts anySatisfy: [:conflict |
		conflict isResolved]