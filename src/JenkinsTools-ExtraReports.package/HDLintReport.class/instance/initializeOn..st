initialization
initializeOn: anEnvironment
	environment := anEnvironment.
	rules := (RBCompositeLintRule rulesFor: RBBasicLintRule)
		reject: [ :each | each class name endsWith: 'SpellingRule' ]