Extension { #name : #Behavior }

{ #category : #'*Reflectivity' }
Behavior >> metaLinkOptions [
	^{
	#methodDict -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]
