events
themeChanged
	"Also pass on to the submenu if any."

	super themeChanged.
	self subMenu ifNotNil:[ :m | m themeChanged ]