Extension { #name : #ReleaseTest }

{ #category : #'*Manifest-Core' }
ReleaseTest >> assertValidLintRule: aLintRule [
	| checker |
	checker := SmalllintManifestChecker new 
		rule: aLintRule;
		run.
	self 
		assert: aLintRule result isEmpty
		description: [ aLintRule rationale ]
]
