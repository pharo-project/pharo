tests
testShouldGetUnresolvedDependencies
	| result |
	
	result := (checker unresolvedDependenciesOf: 'Tool-DependencyAnalyser-Test-Data').
	
	self 	
		assert: result size
		equals: 1.
		
	self assert: ((result at: #collect:as:) includesAllOf: #(#'GT-Spotter' #'Collections-Abstract')).
	