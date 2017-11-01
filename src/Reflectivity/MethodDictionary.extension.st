Extension { #name : #MethodDictionary }

{ #category : #'*Reflectivity' }
MethodDictionary >> metaLinkOptions [
	^{
	#scanFor: -> #( + optionCompileOnLinkInstallation).
	#at:put: -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]
