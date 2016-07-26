settings
fromConfiguration: tree
	"Update the settings from the given tree."
	
	(tree at: #windowColor ifAbsent: []) ifNotNil: [:v | self windowColor: v].
	(tree at: #autoSelectionColor ifAbsent: []) ifNotNil: [:v | self autoSelectionColor: v].
	(tree at: #selectionColor ifAbsent: []) ifNotNil: [:v | self selectionColor: v]