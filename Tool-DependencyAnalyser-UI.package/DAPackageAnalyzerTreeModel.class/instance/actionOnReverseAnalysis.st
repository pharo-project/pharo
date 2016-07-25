protocol
actionOnReverseAnalysis
	buttonReverseAnalysis action: [ 
		(DAPackageAnalyzerPackageDependenciesWindow new 
						initializeWithRelationGraph: self relationGraph) 
					open ]