stand out
findSelectionFor: navigationContext
	^ self
		calculateBestSeletionFor: (self brotherNodeFor: navigationContext bestNodeForSelection )
		beforeSelection: navigationContext textSelection 
		step: [ :selection :target | self selectionBrotherScope: selection for: target ]