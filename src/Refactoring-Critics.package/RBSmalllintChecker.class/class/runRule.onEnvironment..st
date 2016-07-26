instance creation
runRule: aLintRule onEnvironment: aBrowserEnvironment
	aLintRule resetResult.
	(self new)
		rule: aLintRule;
		environment: aBrowserEnvironment;
		run.
	^aLintRule