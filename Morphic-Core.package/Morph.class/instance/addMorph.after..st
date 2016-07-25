submorphs-add/remove
addMorph: newMorph after: aMorph
	"Add the given morph as one of my submorphs, inserting it after anotherMorph"
	^self privateAddMorph: newMorph atIndex: (submorphs indexOf: aMorph)+1