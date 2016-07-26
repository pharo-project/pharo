as yet unclassified
hasAnyNonKeptConflicts
	"Answer whether there are any conflicts not marked as keep."

	^self allConflicts anySatisfy: [:conflict |
		conflict isResolved not or: [conflict remoteChosen]]