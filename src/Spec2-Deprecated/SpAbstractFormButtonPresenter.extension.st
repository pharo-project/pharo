Extension { #name : #SpAbstractFormButtonPresenter }

{ #category : #'*Spec2-Deprecated' }
SpAbstractFormButtonPresenter >> whenActivationActionChanged: aBlock [
	self deprecated: 'Use #whenActivationActionChangedDo: instead.' transformWith: '`@receiver whenActivationActionChanged: `@statements' -> '`@receiver whenActivationActionChangedDo: `@statements'.
	self whenActivationActionChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractFormButtonPresenter >> whenDeactivationActionChanged: aBlock [
	self deprecated: 'Use #whenDeactivationActionChangedDo: instead.' transformWith: '`@receiver whenDeactivationActionChanged: `@statements' -> '`@receiver whenDeactivationActionChangedDo: `@statements'.
	self whenDeactivationActionChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractFormButtonPresenter >> whenLabelClickableChanged: aBlock [
	self deprecated: 'Use #whenLabelClickableChangedDo: instead.' transformWith: '`@receiver whenLabelClickableChanged: `@statements' -> '`@receiver whenLabelClickableChangedDo: `@statements'.
	self whenLabelClickableChangedDo: aBlock
]
