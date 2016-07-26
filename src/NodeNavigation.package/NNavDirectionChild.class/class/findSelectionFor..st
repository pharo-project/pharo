stand out
findSelectionFor: navigationContext
	^ self
		calculateBestSeletionFor: (self childNodeFor: navigationContext bestNodeForSelection)
		beforeSelection: navigationContext textSelection
		step: [ :selection :target | self selectionChildrenScope: selection for: target ]