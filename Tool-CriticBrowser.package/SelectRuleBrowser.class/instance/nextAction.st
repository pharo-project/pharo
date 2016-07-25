api
nextAction

	| rules rbRule |
	self delete.
	rules := selectedModel roots.
	rules do: [ :each | 
				each leaves
					do: [ :leave | 
						(selectedRules anySatisfy: [ :r | r = leave ])
							ifFalse: [ each removeRule: leave ] ] ].
	rules := rules reject: [ :each | each rules isEmpty ].
	rbRule := RBCompositeLintRule new
				name: self defaultRulesRootName;
				rules: rules;
				yourself.
	
	CriticWorkingConfiguration current
		rule: rbRule;
		environment: environment;
		removeTestCase: removeTestCase.
	CriticBrowser openOnWorkingConfiguration: CriticWorkingConfiguration current