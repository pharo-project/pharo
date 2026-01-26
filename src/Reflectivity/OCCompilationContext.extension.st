Extension { #name : 'OCCompilationContext' }

{ #category : '*Reflectivity' }
OCCompilationContext >> metaLinkOptions [
	^{
	#parseOptions: -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]
