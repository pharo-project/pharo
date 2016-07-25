tests
testDependenciesWithoutExtensionDependency
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency
		from: (packageA)
		to: (packageB).
	aCompositeDependency
		add:
			(DAInheritanceDependency from: (packageA) to: (packageB));
		add:
			(DAInheritanceDependency from: (packageA) to: (packageB));
		add:
			(DAExtensionDependency from: (packageA) to: (packageB)).
	self assert: (aCompositeDependency dependenciesWithoutExtensionDependency size = 2).
		