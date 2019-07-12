Extension { #name : #SpMenuPresenter }

{ #category : #'*Spec2-Deprecated' }
SpMenuPresenter >> addTitle: aString [
	self deprecated: 'Use #title: instead.' transformWith: '`@receiver addTitle: `@argument' -> '`@receiver title: `@argument'.
	self title: aString
]
