operations
mergeBaseBetween: aLGitId and: aLGitId2 
	| base |
	base := LGitId new.
	self withReturnHandlerDo: [ self merge_base: base between: aLGitId and: aLGitId2 ].
	^ base