Extension { #name : #SpSelectEntity }

{ #category : #'*Spec2-Deprecated' }
SpSelectEntity >> ghostText [
	self deprecated: 'This API is too tight to Morph. Use #placeholder instead.' transformWith: '`@receiver ghostText' -> '`@receiver placeholder'.
	^ self placeholder
]

{ #category : #'*Spec2-Deprecated' }
SpSelectEntity >> ghostText: aText [
	self deprecated: 'This API is too tight to Morph. Use #placeholder: instead.' transformWith: '`@receiver ghostText: `@statement' -> '`@receiver placeholder: `@statement'.
	self placeholder: aText
]

{ #category : #'*Spec2-Deprecated' }
SpSelectEntity >> whenGhostTextChanged: aBlock [
	self deprecated: 'This API is too tight to Morph. Use #whenPlaceholderChanged: instead.' transformWith: '`@receiver whenGhostTextChanged: `@statement' -> '`@receiver whenPlaceholderChangedDo: `@statement'.
	self whenPlaceholderChangedDo: aBlock
]
