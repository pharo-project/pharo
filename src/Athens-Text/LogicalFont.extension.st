Extension { #name : #LogicalFont }

{ #category : #'*Athens-Text' }
LogicalFont >> asFreetypeFont [
	^ self realFont asFreetypeFont
]

{ #category : #'*Athens-Text' }
LogicalFont >> getGlyphWidth: aCharacter [ 
	^ self realFont getGlyphWidth: aCharacter
]

{ #category : #'*Athens-Text' }
LogicalFont >> getPreciseAscent [
	^ self realFont getPreciseAscent
]

{ #category : #'*Athens-Text' }
LogicalFont >> getPreciseDescent [
	^ self realFont getPreciseDescent
]

{ #category : #'*Athens-Text' }
LogicalFont >> getPreciseHeight [
	^ self realFont getPreciseHeight
]

{ #category : #'*Athens-Text' }
LogicalFont >> glyphRendererOn: anAthensBalloonSurface [ 
	^ self realFont glyphRendererOn: anAthensBalloonSurface
]
