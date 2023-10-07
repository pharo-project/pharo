Extension { #name : 'FreeTypeSystemSettings' }

{ #category : '*FreeType-Graphics' }
FreeTypeSystemSettings class >> loadFt2Library: aBoolean [
	(LoadFT2Library = aBoolean)
		ifTrue: [ ^ self ].
	LoadFT2Library := aBoolean.
	aBoolean
		ifTrue: [
			FreeTypeFontProvider current updateFromSystem ]
		ifFalse: [
			StandardFonts setSmallBitmapFonts.
			FreeTypeFontProvider unload ]
]
