testing-environments
testPragmaEnvironment
	| aPragmaEnvironment |
	aPragmaEnvironment := universalEnvironment forPragmas: #(primitive:).
	self universalTestFor: aPragmaEnvironment