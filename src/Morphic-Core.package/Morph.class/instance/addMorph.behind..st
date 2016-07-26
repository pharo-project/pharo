submorphs-add/remove
addMorph: newMorph behind: aMorph
	"Add a morph to the list of submorphs behind the specified morph"
	^self privateAddMorph: newMorph atIndex: (submorphs indexOf: aMorph) + 1.
