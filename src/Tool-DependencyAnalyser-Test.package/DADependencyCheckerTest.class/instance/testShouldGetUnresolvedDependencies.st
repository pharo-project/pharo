tests
testShouldGetUnresolvedDependencies
	| result |
	
	result := (checker unresolvedDependenciesOf: 'Tool-DependencyAnalyser-Test-Data').
	
	self 	
		assert: result size
		equals: 2.
	self 
		assertCollection: result
		equals: { 
			#asClass->#('System-Support') asSet .
			#collect:as:->#(#'GT-Spotter' #'Collections-Abstract') asSet } asDictionary