submorphs-accessing
noteNewOwner: aMorph 
	"I have just been added as a submorph of aMorph"
	super noteNewOwner: aMorph.

	self submorphs
		do: [:each | each adjustLayoutBounds].
