Extension { #name : #SpAbstractWidgetPresenter }

{ #category : #'*Spec2-Deprecated' }
SpAbstractWidgetPresenter >> enabled [
	self deprecated: 'Please use #isEnabled instead' transformWith: '`@receiver enabled' -> '`@receiver isEnabled'.

	^ self isEnabled
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractWidgetPresenter >> whenBorderColorChanged: aBlock [
	self deprecated: 'Use #whenBorderColorChangedDo: instead.' transformWith: '`@receiver whenBorderColorChanged: `@statements' -> '`@receiver whenBorderColorChangedDo: `@statements'.
	self whenBorderColorChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractWidgetPresenter >> whenBorderWidthChanged: aBlock [
	self deprecated: 'Use #whenBorderWidthChangedDo: instead.' transformWith: '`@receiver whenBorderWidthChanged: `@statements' -> '`@receiver whenBorderWidthChangedDo: `@statements'.
	self whenBorderWidthChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractWidgetPresenter >> whenEnabledChanged: aBlock [
	self deprecated: 'Use #whenEnabledChangedDo: instead.' transformWith: '`@receiver whenEnabledChangedDo: `@statements' -> '`@receiver whenEnabledChangedDo: `@statements'.
	self whenEnabledChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpAbstractWidgetPresenter >> whenHelpChanged: aBlock [
	self deprecated: 'Use #whenHelpChangedDo: instead.' transformWith: '`@receiver whenHelpChanged: `@statements' -> '`@receiver whenHelpChangedDo: `@statements'.
	self whenHelpChangedDo: aBlock
]
