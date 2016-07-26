selection change
autoMultiSelect: aTargetMorph
	autoTargetMorph := aTargetMorph.
	self autoAction
		ifNotNil: [:act |
			self
				from: firstClickedMorph
				to: aTargetMorph
				do: [:nd | self perform: act with: nd].
			self selectionUpdateFromViewWhile: [ | selHolder | 
				selHolder := self newSelectionHolderWithNodePath: nil.
				self listModel selection: selHolder].
			self selectionChanged.
			self scrollToShow: aTargetMorph bounds]