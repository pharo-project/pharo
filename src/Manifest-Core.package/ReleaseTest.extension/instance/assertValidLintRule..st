*Manifest-Core
assertValidLintRule: aLintRule
	| checker |
	checker := SmalllintManifestChecker new 
		rule: aLintRule;
		run.
	self 
		assert: aLintRule result isEmpty
		description: [ aLintRule rationale ]