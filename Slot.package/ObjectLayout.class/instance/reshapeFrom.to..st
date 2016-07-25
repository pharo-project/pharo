reshaping
reshapeFrom: oldScope to: newParentLayout
	"Answer the reshaped version of me for a new parent layout."

	| newScope |
	newScope := self slotScope 
		rebase: oldScope 
		to: newParentLayout slotScope.
	^ newParentLayout extendAgain: self with: newScope.