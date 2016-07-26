accessing
lintChecks
	^ self rules: (self rulesGroupedFor: RBBasicLintRule) name: 'Lint checks'