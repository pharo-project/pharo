instance-creation
newPatchMorph
	"Answer a new patch morph."

	^PSMCMergePatchMorph new
		borderWidth: 0;
		addDependent: self;
		yourself