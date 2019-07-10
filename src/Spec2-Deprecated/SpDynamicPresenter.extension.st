Extension { #name : #SpDynamicPresenter }

{ #category : #'*Spec2-Deprecated' }
SpDynamicPresenter >> widgets [
	self deprecated: 'Use #basicPresenters instead' transformWith: '`@receiver widgets' -> '`@receiver basicPresenters'.
	^ self basicPresenters
]

{ #category : #'*Spec2-Deprecated' }
SpDynamicPresenter >> widgetsDo: aBlock [
	self deprecated: 'Use #presentersDo: instead' transformWith: '`@receiver widgetsDo: `@statements1' -> '`@receiver presentersDo: `@statements1'.
	self presentersDo: aBlock
]
