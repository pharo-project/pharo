add/remove
removeRule: aRule

	{ critics . toDos . falsePositives } do: [ :ruleDict |
		ruleDict removeKey: aRule ]