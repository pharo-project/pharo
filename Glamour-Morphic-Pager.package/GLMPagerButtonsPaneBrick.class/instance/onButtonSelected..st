buttons-event-handling
onButtonSelected: ann

	"notify adapter that we need to update pane preview"
	GLMPagerPanePreviewMorph uniqueInstance showAt: (self buttons at: ann buttonModel index) globalBounds topCenter.
	self pagerModel updatePreview: ann buttonModel index