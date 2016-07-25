testing - navigation
testMethodsReferencingClass
	self assert: (RBLintRuleTest methodsReferencingClass: WriteStream) equals: {(RBLintRuleTest >> #displayName)}.
	self
		assert: (RBLintRuleTest methodsReferencingClass: Object)
		equals:
			{(RBLintRuleTest >> #run).
			(RBLintRuleTest >> #runOnEnvironment:)}.
	self assert: (RBCompositeLintRuleTest class methodsReferencingClass: RBBasicLintRuleTest) equals: {RBCompositeLintRuleTest class>>#lintChecks}