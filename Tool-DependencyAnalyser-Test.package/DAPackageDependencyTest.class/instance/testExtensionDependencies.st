tests
testExtensionDependencies
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency
		from: (packageA)
		to: (packageB).
	aCompositeDependency
		add:
			(DAExtensionDependency from: (packageA) to: (packageB));
		add:
			(DAInheritanceDependency from: (packageA) to: (packageB)).
	self assert: (aCompositeDependency extensionDependencies allSatisfy: [ :each | each isExtensionDependency ]).