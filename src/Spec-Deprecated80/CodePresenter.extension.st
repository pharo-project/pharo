Extension { #name : #CodePresenter }

{ #category : #'*Spec-Deprecated80' }
CodePresenter >> aboutToStyle: aBoolean [
	self deprecated: 'Use #withSyntaxHighlight, #withoutSyntaxHighlight or #syntaxHighlight: instead.' transformWith: '`@receiver aboutToStyle: `@argument' -> '`@receiver syntaxHighlight: `@argument'.
	self syntaxHighlight: aBoolean
]
