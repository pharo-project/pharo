actions
themeChanged
	"Update the colour if specified."

	self color: (self theme menuColorFor: nil).
	super themeChanged