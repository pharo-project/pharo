tests
testReferenceDependencies
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency from: packageA to: packageB.
	aCompositeDependency
		add: (self referenceDependency: String);
		add: (self referenceDependency: Object);
		add: (DAInheritanceDependency from: packageA to: packageB).
		
	self assert: (aCompositeDependency referenceDependencies size = 2).