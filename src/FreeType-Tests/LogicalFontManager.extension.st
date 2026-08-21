Extension { #name : 'LogicalFontManager' }

{ #category : '*FreeType-Tests' }
LogicalFontManager >> identityIncludesFontProvider: aFreeTypeFontProvider [

	^ self fontProviders identityIncludes: aFreeTypeFontProvider
]
