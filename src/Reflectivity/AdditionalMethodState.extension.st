Extension { #name : #AdditionalMethodState }

{ #category : #'*Reflectivity' }
AdditionalMethodState >> metaLinkOptions [
	^{
	#selector -> #( + optionCompileOnLinkInstallation).
	#isMethodProperties -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]
