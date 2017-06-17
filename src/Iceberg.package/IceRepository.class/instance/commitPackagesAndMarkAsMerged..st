actions - utility
commitPackagesAndMarkAsMerged: aCommitish
	^ self 
		commitPackages: self modifiedPackages
		withMessage: 'Merging with ', aCommitish id
		andParents: { self headCommit . aCommitish }