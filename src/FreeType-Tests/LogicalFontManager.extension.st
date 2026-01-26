Extension { #name : 'LogicalFontManager' }

{ #category : '*FreeType-Tests' }
LogicalFontManager >> identityIncludesFontProvider: aFreeTypeFontProvider [
	^ fontProviders identityIncludes: aFreeTypeFontProvider
]
