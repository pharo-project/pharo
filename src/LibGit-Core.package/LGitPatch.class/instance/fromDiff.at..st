operations
fromDiff: aDiff at: anInteger
	"use 1 based offset as in Smalltalk"
	self withReturnHandlerDo: [
		self
			patch_from_diff: self
			diff: aDiff
			idx: anInteger - 1 ].
	self autoRelease