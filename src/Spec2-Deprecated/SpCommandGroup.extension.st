Extension { #name : #SpCommandGroup }

{ #category : #'*Spec2-Deprecated' }
SpCommandGroup >> asToolBarPresenter [
	self
		deprecated: 'Use #asToolbarPresenter instead.'
		transformWith: '`@receiver asToolBarPresenter' -> '`@receiver asToolbarPresenter'.
	^ self asToolbarPresenter
]

{ #category : #'*Spec2-Deprecated' }
SpCommandGroup >> asToolBarPresenterWith: aBlock [
	self
		deprecated: 'Use #asToolbarPresenterWith: instead.'
		transformWith: '`@receiver asToolBarPresenterWith: `@arg' -> '`@receiver asToolbarPresenterWith: `@arg'.
	
	^ self asToolbarPresenterWith: aBlock
]
