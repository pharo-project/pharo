private
copyFor: aBrowserEnvironment 
	| newEnvironment |
	newEnvironment := aBrowserEnvironment copy.
	self assert: newEnvironment numberSelectors = aBrowserEnvironment numberSelectors.
	self assert: (newEnvironment not & aBrowserEnvironment) numberSelectors = 0