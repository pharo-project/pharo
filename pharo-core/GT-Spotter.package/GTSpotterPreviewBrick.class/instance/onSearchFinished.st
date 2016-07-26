event handling
onSearchFinished

	self spotterModel currentStep candidates isEmpty
		ifTrue: [ self updatePreviewWithEmpty ]