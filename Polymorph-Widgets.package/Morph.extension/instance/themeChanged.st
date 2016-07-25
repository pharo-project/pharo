*Polymorph-Widgets
themeChanged
	"The current theme has changed.
	Update any dependent visual aspects."

	self submorphsDo: [:m | m themeChanged].
	self changed