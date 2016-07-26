display
newContentMorph
	| trees unselectedItemsCol selectedItemsCol buttons |
	buttons := (self newGroupboxFor: (
		(self newColumn: {
			self buildAddAllButton.
			self buildAddButton.
			self buildRemoveButton.
			self buildRemoveAllButton})
			vResizing: #shrinkWrap))
		vResizing: #shrinkWrap.
	
	unselectedItemsCol := self newGroupbox: self unselectedLabel for: (self newColumn: {
							self buildUnselectedItemsSearchingTextArea: self.
							self buildUnselectedItemsList.}).
	selectedItemsCol := self newGroupbox: self selectedLabel for: (self newColumn: {
							self buildSelectedItemsSearchingTextArea: self.
							self buildSelectedItemsList.}).
	trees := self newRow: {
					unselectedItemsCol.
					"self newGroupboxFor: self buildUnselectedItemsTree."
					buttons hResizing: #shrinkWrap.
					selectedItemsCol}.
	^trees vResizing: #spaceFill.
