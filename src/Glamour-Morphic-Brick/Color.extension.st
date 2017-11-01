Extension { #name : #Color }

{ #category : #'*Glamour-Morphic-Brick' }
Color >> asRGBInteger [

	^(self red * 255) asInteger + ((self green * 255) asInteger << 8) + ((self blue * 255) asInteger << 16)
]

{ #category : #'*Glamour-Morphic-Brick' }
Color class >> brickColorRegistry [

	^ ColorRegistry
]
