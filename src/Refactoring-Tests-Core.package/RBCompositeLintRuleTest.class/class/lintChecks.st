all checks
lintChecks
	^ self 
		rules: (RBBasicLintRuleTest protocolsToCheck collect: 
			[ :each | 
			self 
				ruleFor: RBBasicLintRuleTest
				protocol: each ])
		name: 'Lint checks'