Extension { #name : #ReleaseTest }

{ #category : #'*Refactoring-Critics' }
ReleaseTest >> assertValidLintRule: aLintRule withExceptions: someNames [
	| runner results |
	runner := ReSmalllintChecker new.
	runner 
		rule: {aLintRule};
		environment: RBBrowserEnvironment default;
		run.
		
	results := (runner criticsOf: aLintRule) reject: [ :critique | someNames includes: critique sourceAnchor entity name  ].
		
	self
		assert: results isEmpty
		description: [ aLintRule rationale ]
]

{ #category : #'*Refactoring-Critics' }
ReleaseTest >> testExplicitRequirementMethodsShouldBeImplementedInTheirUsers [
	"If a class is using a trait with an explicit requirement method, this class should implement the method"
	
	self assertValidLintRule: RBExplicitRequirementMethodsRule new
	
]

{ #category : #'*Refactoring-Critics' }
ReleaseTest >> testLocalMethodsOfTheClassShouldNotBeRepeatedInItsTraits [
	"If a class is using a trait with an explicit requirement method, this class should implement the method"
	
	self assertValidLintRule: RBLocalMethodsSameThanTraitRule new withExceptions: #(RBSmalllintTestLocalMethodsSameThanTraitObject).
]
