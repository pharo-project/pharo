access
addTab: aStringOrTextOrMorph withAction: aSymbolOrBlock
	"Add a new tab.  The tab will be added onto the end of the list and displayed on the far right of previously added tabs.  The first argument can be a simple String, a Text, or any Morph.  The second argument is the action to be performed when the tab is selected. It can either be a symbol for a unary method on the target object or a block.  Each tab is stored as an Association with the created tab as the key and the selector as the value."
	| tabMorph |
	tabMorph := PluggableTabButtonMorph on: nil label: [ aStringOrTextOrMorph].
	tabMorph color: self color.
	self addMorphBack: tabMorph.
	self tabs ifEmpty: [ self activeTab: tabMorph ].
	self tabs add: (Association key: tabMorph value: aSymbolOrBlock).
	self layoutChanged.
	self changed.