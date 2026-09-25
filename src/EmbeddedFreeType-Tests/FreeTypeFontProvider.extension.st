Extension { #name : 'FreeTypeFontProvider' }

{ #category : '*EmbeddedFreeType-Tests' }
FreeTypeFontProvider >> includesInstaller: anEmbeddedFreeTypeFontInstaller [

	^ self fontInstallers identityIncludes: anEmbeddedFreeTypeFontInstaller
]
