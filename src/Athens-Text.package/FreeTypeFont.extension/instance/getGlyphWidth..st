*Athens-Text
getGlyphWidth: aCharacter 
	aCharacter asciiValue = 9 ifTrue: [ 
		^  (self linearWidthOf: Character space) * 8
		]. 
	^ (self linearWidthOf: aCharacter)