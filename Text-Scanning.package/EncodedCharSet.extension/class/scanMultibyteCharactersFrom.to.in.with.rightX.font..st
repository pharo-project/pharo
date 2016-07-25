*Text-Scanning
scanMultibyteCharactersFrom: startIndex to: stopIndex in: aWideString with: aCharacterScanner rightX: rightX font: aFont
	"the default for scanning multibyte characters- other more specific encodings may do something else"
	^aFont scanMultibyteCharactersFrom: startIndex to: stopIndex in: aWideString with: aCharacterScanner rightX: rightX