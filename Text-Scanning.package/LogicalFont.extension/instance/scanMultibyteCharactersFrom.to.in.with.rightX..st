*Text-Scanning
scanMultibyteCharactersFrom: startIndex to: stopIndex in: aWideString with: aCharacterScanner rightX: rightX
	"scan a multibyte character string"
	^self realFont scanMultibyteCharactersFrom: startIndex to: stopIndex in: aWideString with: aCharacterScanner rightX: rightX