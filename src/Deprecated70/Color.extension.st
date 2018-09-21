Extension { #name : #Color }

{ #category : #'*Deprecated70' }
Color >> asNontranslucentColor [
	self
		deprecated: 'Please use #nontranslucentColor instead'
		transformWith: '`@receiver asNontranslucentColor' -> '`@receiver nontranslucentColor'.
	^ self nontranslucentColor
]

{ #category : #'*Deprecated70' }
Color >> isTranslucentColor [
	self
		deprecated: 'use isTranslucentButNotTransparent or isTranslucent'
		transformWith: '`@receiver isTranslucentColor' -> '`@receiver isTranslucentButNotTransparent'.
	^ self isTranslucentButNotTransparent
]
