operations
hunkAt: anInteger
	| hunk lines |
	hunk := LGitDiffHunk externalNew.
	lines := LGitWriteBackValueUInt new.
	self withReturnHandlerDo: [
		self
			patch_get_hunk: hunk
			lines_in_hunk: lines patch: self
			hunk_idx: anInteger ].
		
	^ hunk