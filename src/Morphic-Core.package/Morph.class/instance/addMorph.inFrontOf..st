submorphs-add/remove
addMorph: newMorph inFrontOf: aMorph
	"Add a morph to the list of submorphs in front of the specified morph"
	^self privateAddMorph: newMorph atIndex: ((submorphs indexOf: aMorph) max: 1).