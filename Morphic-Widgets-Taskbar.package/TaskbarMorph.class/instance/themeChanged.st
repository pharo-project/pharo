theme
themeChanged
	"The theme has changed. Update our appearance."

	self initializeAppearance.
	self removeAllMorphs.
	super themeChanged.
	self updateTaskButtons