Extension { #name : #SpCodePresenter }

{ #category : #'*Spec2-Deprecated' }
SpCodePresenter >> aboutToStyle: aBoolean [
	self deprecated: 'Use #withSyntaxHighlight, #withoutSyntaxHighlight or #syntaxHighlight: instead.' transformWith: '`@receiver aboutToStyle: `@argument' -> '`@receiver syntaxHighlight: `@argument'.
	self syntaxHighlight: aBoolean
]
