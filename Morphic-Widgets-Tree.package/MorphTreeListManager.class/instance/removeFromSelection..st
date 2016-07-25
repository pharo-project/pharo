selection change
removeFromSelection: aMorph
	aMorph selected ifFalse: [^false].
	aMorph selected: false.
	aMorph unhighlight.
	self selectionUpdateFromViewWhile: [ | selHolder | 
		selHolder := self newSelectionHolderWithNodePath: nil.
		self listModel selection: selHolder].
	^ true
