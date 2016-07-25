testing
hasValidationIssues
	validationResult == nil ifTrue: [ ^false ].
	^validationResult notEmpty