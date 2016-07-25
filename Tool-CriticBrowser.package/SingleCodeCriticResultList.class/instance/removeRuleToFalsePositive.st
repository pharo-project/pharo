menu
removeRuleToFalsePositive

	criticsModel selectedItems ifNotNil: [ :criticss |
		criticss do: [ :critic |
			cache removeFalsePositiveRule: rule forPackage: critic sourceAnchor entity mcWorkingCopy]].