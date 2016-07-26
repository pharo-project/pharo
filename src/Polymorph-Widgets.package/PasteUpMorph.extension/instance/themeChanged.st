*Polymorph-Widgets
themeChanged
	"The theme has changed.
	Update the desktop wallpaper if appropriate."

	(self theme desktopImageFor: self) ifNotNil: [:aForm |
		self color: Color white.
		self backgroundImage: aForm layout: self theme desktopImageLayout].
	super themeChanged