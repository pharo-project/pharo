Extension { #name : #AbstractFont }

{ #category : #'*FreeType-addition' }
AbstractFont >> displayStrikeoutOn: aDisplayContext from: baselineStartPoint to: baselineEndPoint [
	"display the strikeout if appropriate for the receiver"
]

{ #category : #'*FreeType-addition' }
AbstractFont >> displayUnderlineOn: aDisplayContext from: baselineStartPoint to: baselineEndPoint [
	"display the underline if appropriate for the receiver"
]

{ #category : #'*FreeType-addition' }
AbstractFont >> emphasisString [
	"Answer a translated string that represents the receiver's emphasis."
	
	^self emphasisStringFor: self emphasis
]

{ #category : #'*FreeType-addition' }
AbstractFont >> emphasisStringFor: emphasisCode [
	"Answer a translated string that represents the attributes given in emphasisCode."
	
	^self class emphasisStringFor: emphasisCode
]

{ #category : #'*FreeType-addition' }
AbstractFont >> hasDistinctGlyphsForAll: asciiString [
	"Answer true if the receiver has glyphs for all the characters
	in asciiString and no single glyph is shared by more than one character, false otherwise.
	The default behaviour is to answer true, but subclasses may reimplement"
	
	^true
]

{ #category : #'*FreeType-addition' }
AbstractFont >> hasGlyphsForAll: asciiString [
	"Answer true if the receiver has glyphs for all the characters
	in asciiString, false otherwise.
	The default behaviour is to answer true, but subclasses may reimplement"
	
	^true
]

{ #category : #'*FreeType-addition' }
AbstractFont >> isSubPixelPositioned [
	"Answer true if the receiver is currently using subpixel positioned
	glyphs, false otherwise. This affects how padded space sizes are calculated
	when composing text. 
	Currently, only FreeTypeFonts are subPixelPositioned, and only when not
	Hinted"
	
	^false 
]

{ #category : #'*FreeType-addition' }
AbstractFont >> isSymbolFont [
	"Answer true if the receiver is a Symbol font, false otherwise.
	The default is to answer false, subclasses can reimplement"
	
	^false
]

{ #category : #'*FreeType-addition' }
AbstractFont >> kerningLeft: leftChar right: rightChar [
	^0
]

{ #category : #'*FreeType-addition' }
AbstractFont >> linearWidthOf: aCharacter [
	"This is the scaled, unrounded advance width."
	^self widthOf: aCharacter
]

{ #category : #'*FreeType-addition' }
AbstractFont >> widthAndKernedWidthOfLeft: leftCharacter right: rightCharacterOrNil into: aTwoElementArray [
	"Set the first element of aTwoElementArray to the width of leftCharacter and 
	the second element to the width of left character when kerned with
	rightCharacterOrNil. Answer aTwoElementArray"
	| w k |
	w := self widthOf: leftCharacter.
	rightCharacterOrNil 
		ifNil: [
			aTwoElementArray 
				at: 1 put: w; 
				at: 2 put: w]
		ifNotNil: [
			k := self kerningLeft: leftCharacter right: rightCharacterOrNil.
			aTwoElementArray 
				at: 1 put: w; 
				at: 2 put: w+k].
	^ aTwoElementArray
	
]
