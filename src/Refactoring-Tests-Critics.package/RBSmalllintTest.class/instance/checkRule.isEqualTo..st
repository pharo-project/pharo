private
checkRule: aLintRule isEqualTo: anEnvironment 
	| returnedEnvironment |
	returnedEnvironment := aLintRule result.
	self compare: returnedEnvironment to: anEnvironment.
	self compare: anEnvironment to: returnedEnvironment