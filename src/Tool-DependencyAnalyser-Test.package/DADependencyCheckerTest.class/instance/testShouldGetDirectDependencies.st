tests
testShouldGetDirectDependencies
	self 	
		assert: (checker dependenciesOf: 'Tool-DependencyAnalyser-Test-Data')
		equals: #(#Kernel)