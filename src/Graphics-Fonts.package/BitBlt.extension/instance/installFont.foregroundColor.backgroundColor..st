*Graphics-Fonts
installFont: aFont foregroundColor: foregroundColor backgroundColor: backgroundColor
	"Double dispatch into the font. This method is present so that other-than-bitblt entities can be used by CharacterScanner and friends to display text."
	^aFont installOn: self foregroundColor: foregroundColor backgroundColor: backgroundColor