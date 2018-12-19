Extension { #name : #LogicalFontManager }

{ #category : #'*FontInfrastructure-Tests' }
LogicalFontManager >> identityIncludesFontProvider: aFreeTypeFontProvider [ 
	^ fontProviders identityIncludes: aFreeTypeFontProvider
]
