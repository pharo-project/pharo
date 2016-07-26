initialization
updateShortcutList
	| cat items |
	cat := categories value at: self categoryList selectedIndex.
	items := (cat entriesAt: #all) keymaps collect: [ :e | collectingBlock value: e ].
	shortcutList items: (items sorted: sortingBlock)