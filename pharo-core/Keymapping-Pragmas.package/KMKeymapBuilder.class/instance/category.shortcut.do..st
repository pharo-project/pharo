building
category: aCategoryName shortcut: aShortcutCombination do: actionBlock
	"actionBlock - can take 3 optional arguments: target, morph, and keyboard event"
	aShortcutCombination combinationsDo: [ :combination |
		KMRepository default
			initializeKeymap: shortcutName
			executingOn: combination shortcut
			doing: actionBlock
			inCategory: aCategoryName
			platform: combination platform
	]