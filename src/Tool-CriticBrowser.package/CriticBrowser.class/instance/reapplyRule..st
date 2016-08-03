private
reapplyRule: aRule

	|  oldCritics |
	oldCritics := aRule critics.
	checker resetResult.
	rbEnvironment packages do: [ :package | 
		checker runRules: { aRule } onPackage: package withoutTestCase: removeTestCase ]. 	
	(oldCritics \ (aRule critics)) do: [ :each |
		cache removeCritic: each forRule: aRule.
		cache removeFalsePositive: each forRule: aRule.
		cache removeToDo: each forRule: aRule ].
	checker rule: aRule. 