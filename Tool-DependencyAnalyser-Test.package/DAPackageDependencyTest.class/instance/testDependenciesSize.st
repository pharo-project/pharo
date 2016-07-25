tests
testDependenciesSize
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency from: packageA to: packageB.
	aCompositeDependency
		add: (DAInheritanceDependency from: packageA to: packageB);
		add: (DAInheritanceDependency from: packageA to: packageB).
	self assert: aCompositeDependency extensionDependenciesSize = 0.
	aCompositeDependency add: (DAExtensionDependency from: packageA to: packageB).
	self assert: aCompositeDependency extensionDependenciesSize = 1