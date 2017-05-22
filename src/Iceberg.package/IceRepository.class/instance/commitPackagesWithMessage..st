actions - utility
commitPackagesWithMessage: message
	self modifiedPackages 
		ifEmpty: [ NothingToCommitException signal ]
		ifNotEmpty: [ :packages |
			self 
				commitPackages: packages
				withMessage: message ]