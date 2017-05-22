operations
patchAt: anInteger
	^ LGitPatch
		fromDiff: self
		at: anInteger