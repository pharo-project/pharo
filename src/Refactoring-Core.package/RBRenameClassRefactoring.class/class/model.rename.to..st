instance creation
model: aRBSmalltalk rename: aClass to: aNewName 
	^(self new)
		model: aRBSmalltalk;
		className: aClass name newName: aNewName;
		yourself