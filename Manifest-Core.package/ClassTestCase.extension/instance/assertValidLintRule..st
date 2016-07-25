*Manifest-Core
assertValidLintRule: aLintRule
	| checker |
	checker := SmalllintManifestChecker new 
		rule: aLintRule; 
		environment: self targetClassEnvironment;
		run.
	self 
		assert: aLintRule result isEmpty
		description: [ aLintRule rationale ]