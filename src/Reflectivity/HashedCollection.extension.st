Extension { #name : #HashedCollection }

{ #category : #'*Reflectivity' }
HashedCollection >> metaLinkOptions [
	^{
	#findElementOrNil: -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]
