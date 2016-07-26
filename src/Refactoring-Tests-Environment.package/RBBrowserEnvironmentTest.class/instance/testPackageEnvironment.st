testing-environments
testPackageEnvironment
	| aPackageEnvironment |
	aPackageEnvironment := universalEnvironment
		forPackageNames: (Array with: 'Refactoring-Tests-Environment' with: 'Refactoring-Tests-Critics').
	self universalTestFor: aPackageEnvironment.
	self assert: (aPackageEnvironment implementorsOf: #testPackageEnvironment) numberSelectors equals: 1