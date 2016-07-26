building
category: aCategoryName default: aShortcut do: actionBlock
	"actionBlock - can take 3 optional arguments: target, morph, and keyboard event"

	KMRepository default
		initializeKeymap: shortcutName
		executingOn: aShortcut
		doing: actionBlock
		inCategory: aCategoryName
		platform: platform