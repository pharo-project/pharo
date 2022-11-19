Extension { #name : #Socket }

{ #category : #'*System-Platforms' }
Socket >> socketErrorMessage [

	^ OSPlatform current getErrorMessage: self socketError
]
