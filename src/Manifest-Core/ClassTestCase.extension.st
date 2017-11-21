Extension { #name : #ClassTestCase }

{ #category : #'*Manifest-Core' }
ClassTestCase >> assertValidLintRule: aLintRule [
	| runner |
	runner := ReSmalllintChecker new.
	runner 
		rule: {aLintRule};
		environment: self targetClassEnvironment;
		run.
	self
		assert: (runner criticsOf: aLintRule) isEmpty
		description: [ aLintRule rationale ]
]

{ #category : #'*Manifest-Core' }
ClassTestCase >> targetClassEnvironment [
	^RBClassEnvironment class: self targetClass.
]
