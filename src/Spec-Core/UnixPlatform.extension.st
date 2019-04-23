Extension { #name : #UnixPlatform }

{ #category : #'*Spec-Core' }
UnixPlatform >> configureApplication: anApplication configuration: aConfiguration [
	
	aConfiguration configureUnix: anApplication
]
