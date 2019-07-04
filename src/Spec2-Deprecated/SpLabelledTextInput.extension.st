Extension { #name : #SpLabelledTextInput }

{ #category : #'*Spec2-Deprecated' }
SpLabelledTextInput >> ghostText: aText [
	self deprecated: 'This API is too tight to Morph. Use #placeholder: instead.' transformWith: '`@receiver ghostText: `@statement' -> '`@receiver placeholder: `@statement'.
	self placeholder: aText
]
