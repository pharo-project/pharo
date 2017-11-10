Extension { #name : #FreeTypeFont }

{ #category : #'*Athens-Text' }
FreeTypeFont >> asFreetypeFont [
	^ self
]

{ #category : #'*Athens-Text' }
FreeTypeFont >> getGlyphWidth: aCharacter [ 
	aCharacter asciiValue = 9 ifTrue: [ 
		^  (self linearWidthOf: Character space) * 8
		]. 
	^ (self linearWidthOf: aCharacter)
]

{ #category : #'*Athens-Text' }
FreeTypeFont >> getPreciseAscent [
	"we using face bouding box for getting ascent value, otherwise two different
	font sizes on same line won't align properly"
	
	^(self face ascender * self pixelSize / self face unitsPerEm) asFloat.

]

{ #category : #'*Athens-Text' }
FreeTypeFont >> getPreciseDescent [
	"we using face bouding box for getting ascent value, otherwise two different
	font sizes on same line won't align properly"
	
	^(self face descender * self pixelSize / self face unitsPerEm) asFloat negated.


]

{ #category : #'*Athens-Text' }
FreeTypeFont >> getPreciseHeight [
	"get precise font ascent, can be floating value"
	
	^(self face height * self pixelSize / self face unitsPerEm) asFloat.

]

{ #category : #'*Athens-Text' }
FreeTypeFont >> glyphRendererOn: anAthensSurface [

	^ anAthensSurface getFreetypeFontRendererFor: self

]
