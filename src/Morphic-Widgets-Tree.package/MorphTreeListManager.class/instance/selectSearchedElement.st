private
selectSearchedElement

	self searchedElement 
		ifNotNil: [ :selectedMorph | 
			self setSelectedMorph: selectedMorph.
			self searchedElement: nil ]