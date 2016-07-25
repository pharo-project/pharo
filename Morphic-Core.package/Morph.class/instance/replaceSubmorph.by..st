submorphs-add/remove
replaceSubmorph: oldMorph by: newMorph
	| index itsPosition w |
	oldMorph stopStepping.
	itsPosition := oldMorph referencePositionInWorld.
	index := submorphs indexOf: oldMorph.
	oldMorph privateDelete.
	self privateAddMorph: newMorph atIndex: index.
	newMorph referencePositionInWorld: itsPosition.
	(w := newMorph world) ifNotNil:
		[w startSteppingSubmorphsOf: newMorph]