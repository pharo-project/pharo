*Reflectivity
metaLinkOptions
	^{
	#scanFor: -> #( + optionCompileOnLinkInstallation).
	#at:put: -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}