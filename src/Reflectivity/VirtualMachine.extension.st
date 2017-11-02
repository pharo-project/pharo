Extension { #name : #VirtualMachine }

{ #category : #'*Reflectivity' }
VirtualMachine >> metaLinkOptions [
	^{
	#isSpur -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]
