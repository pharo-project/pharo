tests
testContainsOnlyExtensionDependency
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency
		from: (packageA)
		to: (packageB).
	aCompositeDependency
		add:
			(DAInheritanceDependency from: (packageA) to: (packageB));
		add:
			(DAInheritanceDependency from: (packageA) to: (packageB)).
	self deny: (aCompositeDependency containsOnlyExtensionDependency).
	aCompositeDependency dependencies removeAll.
		aCompositeDependency
		add:
			(DAExtensionDependency from: (packageA) to: (packageB));
		add:
			(DAExtensionDependency from: (packageA) to: (packageB)).
	self assert: (aCompositeDependency containsOnlyExtensionDependency).
		