Extension { #name : #AbstractFont }

{ #category : #'*Text-Scanning' }
AbstractFont >> scanByteCharactersFrom: startIndex to: stopIndex in: aByteString with: aCharacterScanner rightX: rightX [
	"scan a single byte character string"
	^aCharacterScanner scanByteCharactersFrom: startIndex to: stopIndex in: aByteString rightX: rightX
]

{ #category : #'*Text-Scanning' }
AbstractFont >> scanMultibyteCharactersFrom: startIndex to: stopIndex in: aWideString with: aCharacterScanner rightX: rightX [
	"scan a multibyte character string"
	^aCharacterScanner scanMultibyteCharactersFrom: startIndex to: stopIndex in: aWideString rightX: rightX 
]
