testing
testMatches
	| envEnvironment environmentEnvironment |
	envEnvironment := RBBrowserEnvironment new matches: '*env*'.
	environmentEnvironment := RBBrowserEnvironment new 
				referencesTo: #environment.
	self assert: (envEnvironment referencesTo: #environment) numberSelectors 
				= environmentEnvironment numberSelectors