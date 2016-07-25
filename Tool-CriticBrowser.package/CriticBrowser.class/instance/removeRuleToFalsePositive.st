private
removeRuleToFalsePositive

	rulesModel selectedItem ifNotNil: [ :rule |
		rule leaves do: [ :r |
			rbEnvironment packages do: [ :package |
				 cache removeFalsePositiveRule: r forPackage: package ]]].

	
	