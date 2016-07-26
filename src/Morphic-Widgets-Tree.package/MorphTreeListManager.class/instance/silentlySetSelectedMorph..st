selection change
silentlySetSelectedMorph: aMorph
	| path |
	
	path := aMorph ifNotNil: [ aMorph path collect: [ :m | m complexContents ] ].
	self isMultiple ifFalse: [ self emptySelection ].
	
	aMorph ifNotNil: [ self addToSelection: aMorph ].
	
	self selectionUpdateFromViewWhile: [ 
			self listModel selection: (self newSelectionHolderWithNodePath: path) ]