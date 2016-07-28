testing - navigation
testMethodsReferencingClass
	self assert: (RBLintRuleTest methodsReferencingClass: WriteStream) equals: {(RBLintRuleTest >> #displayName)}.
	self
		assert: ((RBLintRuleTest methodsReferencingClass: Object) sort: [ :a :b | a name <= b name]) asArray
		equals:
			{(RBLintRuleTest >> #run).
			(RBLintRuleTest >> #runOnEnvironment:)}.
	self assert: (RBCompositeLintRuleTest class methodsReferencingClass: RBBasicLintRuleTest) equals: {RBCompositeLintRuleTest class>>#lintChecks}