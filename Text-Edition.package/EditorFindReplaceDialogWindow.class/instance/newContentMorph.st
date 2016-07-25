user-interface
newContentMorph
	^(self newColumn: {	
			(self newGroupbox: nil for: 
				(self newColumn: {
					self newFindTextEntryMorph.
					(self newRow: {
						(self newColumn: {
							(self newCheckboxFor: self getSelected: #isRegex setSelected: #isRegex: getEnabled: nil label: 'Regular expression' translated help: nil).
							(self newCheckboxFor: self getSelected: #caseSensitive setSelected: #caseSensitive: getEnabled: nil label: 'Case sensitive' translated help: nil).
							(self newCheckboxFor: self getSelected: #entireWordsOnly setSelected: #entireWordsOnly: getEnabled: nil label: 'Entire words only'  translated help: nil) }
						).
						(self newColumn: {
							(self newCheckboxFor: self getSelected: #searchBackwards setSelected: #searchBackwards: getEnabled: nil label: 'Search backwards'  translated help: nil).
							(self newCheckboxFor: self getSelected: #wrapAround setSelected: #wrapAround: getEnabled: nil label: 'Wrap around'  translated help: nil) }
						) }	
					) }
				)
			).
			(self newLabelGroup: {'Replace with: ' translated -> self newReplaceTextEntryMorph}) vResizing: #shrinkWrap}
		) 

