selection change
addAllToSelection: aCollection
| selHolder |
	self searchedElement: nil.
	aCollection do: [:m | m 
		highlight;
		selected: true ].
	self selectionChanged.
	self selectionUpdateFromViewWhile: [
		selHolder := self newSelectionHolderWithNodePath: (aCollection last path collect: [:m | m complexContents]).
		self listModel selection: selHolder]
