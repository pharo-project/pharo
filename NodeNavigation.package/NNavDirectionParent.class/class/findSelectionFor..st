stand out
findSelectionFor: navigationContext
	^ self
		calculateBestSeletionFor: navigationContext bestNodeForSelection 
		beforeSelection: navigationContext textSelection 
		step: [ :selection :target | self selectionParentScope: selection for: target parent ]