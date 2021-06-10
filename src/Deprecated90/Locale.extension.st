Extension { #name : #Locale }

{ #category : #'*Deprecated90' }
Locale class >> currentPlatform [
	self deprecated: 'Use #current instead.' transformWith: '`@receiver currentPlatform' -> '`@receiver current'.
	^ self current
]

{ #category : #'*Deprecated90' }
Locale class >> currentPlatform: locale [

		self deprecated: 'Use #switchTo: instead.' transformWith: '`@receiver currentPlatform: `@arg' -> '`@receiver switchTo: `@arg'.
		self switchTo: locale
]
