Extension { #name : #MCCacheRepository }

{ #category : #'*Komitter-Models' }
MCCacheRepository >> isCache [
	"This is a bit hackish but we want to exclude the package cache which does not seem to be instance of a dedicated class"
	
	^ (self description includesSubstring: 'package-cache')
]
