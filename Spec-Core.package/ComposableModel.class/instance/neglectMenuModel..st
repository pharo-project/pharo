api
neglectMenuModel: aMenuModel

	aMenuModel menuGroups do: [ :group |
		group menuItems do: [ :item |
			item shortcut ifNotNil: [ :shortcut | self removeMenuKeyCombination: shortcut ].
			item subMenu ifNotNil: [ :subMenu | subMenu neglect: self	] ] ]