private
assertResultOfRule: aLintRule hasSameClassesAsEnvironment: anEnvironment
	| resultEnvironment |
	resultEnvironment := aLintRule result.
	self assertEnvironment: resultEnvironment hasAllClassesInEnvironment: anEnvironment.
	self assertEnvironment: anEnvironment hasAllClassesInEnvironment: resultEnvironment