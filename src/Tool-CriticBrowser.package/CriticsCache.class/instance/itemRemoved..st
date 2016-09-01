add/remove
itemRemoved: anItem

	{ critics . toDos . falsePositives } do: [ :ruleDict |
		ruleDict valuesDo: [ :crits |
			crits removeAllSuchThat:  [ :crit |
				crit sourceAnchor entity = anItem ] ] ].
	