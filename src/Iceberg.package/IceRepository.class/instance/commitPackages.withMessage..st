actions - utility
commitPackages: packagesToCommit withMessage: message
	^ self 
		commitPackages: packagesToCommit
		withMessage: message
		andParents: { self headCommit }