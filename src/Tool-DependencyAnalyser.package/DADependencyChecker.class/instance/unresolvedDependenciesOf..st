accessing
unresolvedDependenciesOf: aPackageName 
	^ [ (DAMessageSendAnalyzer on: aPackageName) missingMethodsWithPotentialMatchAfterManuallyResolvedDependenciesAddition
		reject: #isEmpty ]
		on: PotentialOutDatedDependencyWarning
		do: [ :ex | ex resume ]