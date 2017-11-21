Extension { #name : #ReleaseTest }

{ #category : #'*Manifest-Core' }
ReleaseTest >> assertValidLintRule: aLintRule [ 
	| runner |
	runner := ReSmalllintChecker new.
	runner 
		rule: {aLintRule};
		environment: RBBrowserEnvironment default;
		run.
	self
		assert: (runner criticsOf: aLintRule) isEmpty
		description: [ aLintRule rationale ]
]
