*Polymorph-Widgets
themeChanged
	"Update the corner style."

	self cornerStyle: (self theme tabGroupCornerStyleIn: self window).
	self pageMorphs do: [ :morph | morph themeChanged ].
	super themeChanged