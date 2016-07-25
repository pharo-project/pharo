menu
addRuleToFalsePositive

	criticsModel selectedItems ifNotNil: [ :criticss |
		criticss do: [ :critic |
			cache addFalsePositiveRule: rule forPackage: critic mcWorkingCopy]].