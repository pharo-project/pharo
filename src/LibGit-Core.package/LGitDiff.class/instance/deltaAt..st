operations
deltaAt: anInteger
	"use 1 based offset as in Smalltalk"
	^ LGitDiffDelta fromHandle: (
		self
			diff_get_delta: self
			idx: anInteger - 1) getHandle