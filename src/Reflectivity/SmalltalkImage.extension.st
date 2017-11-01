Extension { #name : #SmalltalkImage }

{ #category : #'*Reflectivity' }
SmalltalkImage >> metaLinkOptions [
	^{
	#vm -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]
