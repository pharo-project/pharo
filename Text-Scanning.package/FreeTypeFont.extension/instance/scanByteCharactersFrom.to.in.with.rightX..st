*Text-Scanning
scanByteCharactersFrom: startIndex to: stopIndex in: aByteString with: aCharacterScanner rightX: rightX
	"scan a single byte character string"
	^aCharacterScanner scanKernableByteCharactersFrom: startIndex to: stopIndex in: aByteString rightX: rightX