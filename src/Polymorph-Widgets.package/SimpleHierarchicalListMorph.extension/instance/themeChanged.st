*Polymorph-Widgets
themeChanged
	"Update the selection colour."

	self selectionColor: self theme selectionColor.
	self color: self defaultColor.
	super themeChanged