Extension { #name : #FreeTypeFontProvider }

{ #category : #'*EmbeddedFreeType-Tests' }
FreeTypeFontProvider >> includesInstaller: anEmbeddedFreeTypeFontInstaller [ 
	^ fontInstallers identityIncludes: anEmbeddedFreeTypeFontInstaller
]
