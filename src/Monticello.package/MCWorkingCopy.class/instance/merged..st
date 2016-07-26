operations
merged: aVersion
	ancestry addAncestor: aVersion info.
	aVersion dependencies do: [:ea | self requirePackage: ea package].
	self changed