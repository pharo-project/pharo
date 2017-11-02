Extension { #name : #FreeTypeFont }

{ #category : #'*Text-Scanning' }
FreeTypeFont >> scanByteCharactersFrom: startIndex to: stopIndex in: aByteString with: aCharacterScanner rightX: rightX [
	"scan a single byte character string"
	^aCharacterScanner scanKernableByteCharactersFrom: startIndex to: stopIndex in: aByteString rightX: rightX
]

{ #category : #'*Text-Scanning' }
FreeTypeFont >> scanMultibyteCharactersFrom: startIndex to: stopIndex in: aWideString with: aCharacterScanner rightX: rightX [
	"scan a multibyte character string"
	^aCharacterScanner scanKernableMultibyteCharactersFrom: startIndex to: stopIndex in: aWideString rightX: rightX 
]
