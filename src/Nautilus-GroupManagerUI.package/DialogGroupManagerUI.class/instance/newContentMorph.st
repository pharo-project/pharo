item creation
newContentMorph
	^ self
		newColumn:
			{self packageTreeBox.
			self
				newRow:
					{self buildAddSelectedPackageInSelectedGroupButton.
					self newColumn: {self buildRemoveSelectedPackageFromSelectedGroupButton}}.
			(self newRow: {self groupTreeBox})
				hResizing: #spaceFill;
				vResizing: #spaceFill}