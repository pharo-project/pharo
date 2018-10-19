Extension { #name : #Color }

{ #category : #'*Deprecated70' }
Color >> asNontranslucentColor [
	self
		deprecated: 'Please use #opaqueColor instead'
		transformWith: '`@receiver asNontranslucentColor' -> '`@receiver opaqueColor'.
	^ self opaqueColor
]

{ #category : #'*Deprecated70' }
Color >> isTranslucentColor [
	self
		deprecated: 'use isTranslucentButNotTransparent or isTranslucent'
		transformWith: '`@receiver isTranslucentColor' -> '`@receiver isTranslucentButNotTransparent'.
	^ self isTranslucentButNotTransparent
]
