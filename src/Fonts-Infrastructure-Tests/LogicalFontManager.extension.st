Extension { #name : #LogicalFontManager }

{ #category : #'*Fonts-Infrastructure-Tests' }
LogicalFontManager >> identityIncludesFontProvider: aFreeTypeFontProvider [ 
	^ fontProviders identityIncludes: aFreeTypeFontProvider
]
