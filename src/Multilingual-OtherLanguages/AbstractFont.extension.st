Extension { #name : #AbstractFont }

{ #category : #'*Multilingual-OtherLanguages' }
AbstractFont >> scanMultibyteJapaneseCharactersFrom: startIndex to: stopIndex in: aWideString with: aCharacterScanner rightX: rightX [
	"scan a multibyte Japanese character string"
	^aCharacterScanner scanJapaneseCharactersFrom: startIndex to: stopIndex in: aWideString rightX: rightX 

]
