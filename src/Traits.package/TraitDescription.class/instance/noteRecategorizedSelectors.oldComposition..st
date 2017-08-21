organization updating
noteRecategorizedSelectors: aCollection oldComposition: aTraitComposition
	
	aCollection do: [:each | | oldCategory newCategory | 
		oldCategory := self organization categoryOfElement: each.
		newCategory := (self traitComposition methodDescriptionForSelector: each) effectiveMethodCategory.
		self noteRecategorizedSelector: each from: oldCategory to: newCategory]