instance creation
runRule: aLintRule
	(self new)
		rule: aLintRule;
		run.
	^aLintRule