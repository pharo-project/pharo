private
reapplyRule: aRule

	cache removeRule: aRule.
	checker resetResult.
	rbEnvironment packages do: [ :package | 
		checker runRules: { aRule } onPackage: package withoutTestCase: removeTestCase ]. 
	(checker criticsOf: aRule) do:	[ :crit |
		cache addCritic: crit forRule: aRule ].
	(checker falsePositiveOf: aRule) do:	[ :crit |
		cache addFalsePositive: crit forRule: aRule ].
	(checker toDoOf: aRule) do:	[ :crit |
		cache addToDo: crit forRule: aRule ].	
	
	cache updateBrowser