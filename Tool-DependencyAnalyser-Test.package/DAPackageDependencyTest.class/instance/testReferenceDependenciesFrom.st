tests
testReferenceDependenciesFrom
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency from: packageA to: packageB.
		aCompositeDependency
		add: (self referenceDependency: String);
		add: (self referenceDependency: Object);
		add: (self referenceDependency: String).
		
	self assert: ((aCompositeDependency referenceDependenciesFrom: String) size = 2).