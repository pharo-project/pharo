adding
addMessageSendDependencies: aPackage
	| analyser |
	analyser := (DAMessageSendAnalyzer basicNew
		initializeWith: aPackage;
		yourself).
	analyser manuallyResolvedDependencies do: 
		[ :packageName |
		(analyser missingMethodsImplementedIn: packageName) 
			ifEmpty: [ PotentialOutDatedDependencyWarning signalDependency: packageName on: aPackage packageName ]
			ifNotEmpty: 	[ :implementedMethods |
				aPackage add: ((DAMessageSendDependency from: aPackage to: (self daPackageWith: packageName))
					implementedMethods: implementedMethods;
					yourself) ] ]