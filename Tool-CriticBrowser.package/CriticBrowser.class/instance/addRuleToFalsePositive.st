private
addRuleToFalsePositive
	rulesModel selectedItem ifNotNil: [ :rule |
		rule leaves do: [ :r |
			rbEnvironment packages do: [ :package |
				cache addFalsePositiveRule: r forPackage: package ]]].
	