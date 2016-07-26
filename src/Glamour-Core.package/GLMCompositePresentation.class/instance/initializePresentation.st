initialization
initializePresentation

	super initializePresentation.
	self do: [:each| each initializePresentation]