*Polymorph-Widgets
themeChanged
	"The current theme has changed.
	Update any dependent visual aspects."

	scrollBar theme: self theme.
	hScrollBar theme: self theme.
	super themeChanged