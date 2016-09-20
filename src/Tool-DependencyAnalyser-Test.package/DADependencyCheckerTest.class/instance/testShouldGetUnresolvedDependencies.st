tests
testShouldGetUnresolvedDependencies
	| result |
	
	result := (checker unresolvedDependenciesOf: 'Tool-DependencyAnalyser-Test-Data').
	
	self 	
		assert: result size
		equals: 1.
	self 
		assertCollection: result
		equals: { #collect:as: -> #(#'GT-Spotter' #'Collections-Abstract') asSet } asDictionary