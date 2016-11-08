private
assertResultOfRule: aLintRule hasSameSelectorsAsEnvironment: anEnvironment
	| resultEnvironment |
	resultEnvironment := aLintRule result.
	self assertEnvironment: resultEnvironment hasAllSelectorsInEnvironment: anEnvironment.
	self assertEnvironment: anEnvironment hasAllSelectorsInEnvironment: resultEnvironment