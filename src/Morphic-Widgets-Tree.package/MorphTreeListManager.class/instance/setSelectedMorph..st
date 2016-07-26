selection change
setSelectedMorph: aMorph
	| path |
	path := aMorph ifNotNil: [ aMorph path collect: [ :m | m complexContents ] ].
	self lastClickedMorph: aMorph.
	self emptySelection.
	aMorph 
		ifNotNil: [ self addToSelection: lastClickedMorph ].
	self selectionUpdateFromViewWhile: [ 
			self listModel selection: (self newSelectionHolderWithNodePath: path) ]