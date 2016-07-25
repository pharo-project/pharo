item creation
groupTreeBox
	^ GroupboxMorph new
		addContentMorph: self buildGroupsTree;
		addContentMorph:
			(self
				newRow:
					{self buildAddButton.
					self buildRenameButton.
					self buildEmptyGroupButton.
					self buildRemoveButton});
		label: 'Choose a group:';
		hResizing: #spaceFill;
		vResizing: #spaceFill.