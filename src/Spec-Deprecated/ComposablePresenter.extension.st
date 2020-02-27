Extension { #name : #ComposablePresenter }

{ #category : #'*Spec-Deprecated' }
ComposablePresenter >> icon: aSymbol [
	self deprecated: 'Use #iconNamed: instead' transformWith: '`@receiver icon: `@statements' -> '`@receiver iconNamed: `@statements'.
	^ self iconNamed: aSymbol
]

{ #category : #'*Spec-Deprecated' }
ComposablePresenter >> instantiateModels: aCollectionOfPairs [
	
	"instantiateModels: is legacy code in ComposablePresenter and must not be used. It will be deprecated and removed."

	^ self instantiatePresenters: aCollectionOfPairs
]
