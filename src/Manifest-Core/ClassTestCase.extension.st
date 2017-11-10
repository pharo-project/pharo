Extension { #name : #ClassTestCase }

{ #category : #'*Manifest-Core' }
ClassTestCase >> assertValidLintRule: aLintRule [
	| checker |
	checker := SmalllintManifestChecker new 
		rule: aLintRule; 
		environment: self targetClassEnvironment;
		run.
	self 
		assert: aLintRule result isEmpty
		description: [ aLintRule rationale ]
]

{ #category : #'*Manifest-Core' }
ClassTestCase >> targetClassEnvironment [
	^RBClassEnvironment class: self targetClass.
]
