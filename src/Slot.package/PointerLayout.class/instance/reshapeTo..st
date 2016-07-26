reshaping
reshapeTo: aModification
	| newScope |
	newScope := slotScope rebase: aModification originalScope to: aModification newScope.
	^ aModification newLayout extendAgain: self with: newScope.