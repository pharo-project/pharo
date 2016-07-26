items creation
buildSearchingTextArea
	^ searchingTextArea := 
		(EditableDropListMorph
				on: self
				list: #searchedTextList
				selected: #searchingString
				changeSelected: #searchingAccept:
				useIndex: false 
				addToList: #addToSearchedTextList:
				class: String
				getEnabled: nil) hResizing: #spaceFill; 
				ghostText: 'Hit return to accept' translated.

	