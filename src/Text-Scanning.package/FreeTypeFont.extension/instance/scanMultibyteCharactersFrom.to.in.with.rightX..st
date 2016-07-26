*Text-Scanning
scanMultibyteCharactersFrom: startIndex to: stopIndex in: aWideString with: aCharacterScanner rightX: rightX
	"scan a multibyte character string"
	^aCharacterScanner scanKernableMultibyteCharactersFrom: startIndex to: stopIndex in: aWideString rightX: rightX 