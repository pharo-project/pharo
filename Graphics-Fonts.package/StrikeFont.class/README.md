I represent a compact encoding of a set of Forms corresponding to characters in the ASCII character set. All the forms are placed side by side in a large form whose height is the font height, and whose width is the sum of all the character widths. The xTable variable gives the left-x coordinates of the subforms corresponding to the glyphs. Characters are mapped to glyphs by using the characterToGyphMap.

Subclasses can have non-trivial mapping rules as well as different representations for glyphs sizes (e.g., not using an xTable). If so, these classes should return nil when queried for xTable and/or the characterToGlyphMap. This will cause the CharacterScanner primitive to fail and query the font for the width of a character (so that a more programatical approach can be implemented).

For display, fonts need to implement two messages:
	#installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor
This method installs the receiver (a font) on the given DisplayContext (which may be an instance of BitBlt or Canvas (or any of it's subclasses). The font should take the appropriate action to initialize the display context so that further display operations can be optimized.
	#displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta
This method is called for each subsequent run of characters in aString which is to be displayed with the (previously installed) settings.
