language methods
scanMultibyteCharactersFrom: startIndex to: stopIndex in: aWideString with: aCharacterScanner rightX: rightX font: aFont
	"scanning multibyte Japanese strings"
	^aFont scanMultibyteJapaneseCharactersFrom: startIndex to: stopIndex in: aWideString with: aCharacterScanner rightX: rightX