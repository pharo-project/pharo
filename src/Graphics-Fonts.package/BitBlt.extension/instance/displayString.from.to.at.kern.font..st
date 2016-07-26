*Graphics-Fonts
displayString: aString from: startIndex to: stopIndex at: aPoint kern: kernDelta font: aFont
	"Double dispatch into the font. This method is present so that other-than-bitblt entities can be used by CharacterScanner and friends to display text."
	^ aFont displayString: aString on: self from: startIndex to: stopIndex at: aPoint kern: kernDelta