Extension { #name : #Set }

{ #category : #'*Reflectivity' }
Set >> metaLinkOptions [
	^{
	#parseOptions: -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]
