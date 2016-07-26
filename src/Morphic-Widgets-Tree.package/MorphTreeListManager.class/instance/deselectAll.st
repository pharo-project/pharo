selection change
deselectAll
	| selHolder |
	self allNodeMorphs isEmpty
		ifTrue: [^ self].
	self emptySelection.
	self selectionUpdateFromViewWhile: [
		selHolder := self newSelectionHolderWithNodePath: nil.
		self listModel selection: selHolder]

