tests
testDependenciesSize
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency
		from: packageA
		to: packageB.
	aCompositeDependency
		add:
			(DAInheritanceDependency from:  aPackage to: packageB).
	aCompositeDependency
		add:
			(DAInheritanceDependency from:  aPackage to: packageB).
	self assert: (aPackage dependenciesSize = 1).
	aPackage add: aCompositeDependency.
	self assert: (aPackage dependenciesSize = 3).
	