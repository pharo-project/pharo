as yet unclassified
addCharacter: position
	| charWidth fnt |

	currentStrip ifNil: [

		self applyAttributes.
		
		fnt := currentFont font.
		currentLine maxAscent: fnt getPreciseAscent.
		currentLine maxDescent: fnt getPreciseDescent.
		currentLine maxHeight: fnt getPreciseHeight.
		
		currentStrip := AthensCharacterSpan new.
		currentLine addCommand: currentStrip.
		currentStrip start: position.
	].
	
	charWidth := currentFont font getGlyphWidth: (text at: position).
	currentLine addWidth: charWidth.
	currentBackground ifNotNil: [
		currentBackground addWidth: charWidth ].
	
	currentStrip stop: position.
