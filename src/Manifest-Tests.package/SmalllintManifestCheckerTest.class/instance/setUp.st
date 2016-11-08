running
setUp
	| bm |
	self cleaningResources.
	bm := TheManifestBuilder of: MFClassA.
	bm installFalsePositiveOf: RBCodeCruftLeftInMethodsRule uniqueIdentifierName version: 1.
	bm addFalsePositive: MFClassB >> #method3 of: RBCodeCruftLeftInMethodsRule uniqueIdentifierName version: 1.
	bm installToDoOf: GRTemporaryNotReadNorWrittenRule uniqueIdentifierName version: 1.
	bm
		addAllToDo:
			{(MFClassB >> #method3).
			(MFClassA >> #method)}
		of: GRTemporaryNotReadNorWrittenRule uniqueIdentifierName
		version: 1.
	checker := ReSmalllintChecker new