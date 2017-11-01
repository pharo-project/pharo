Extension { #name : #ComposablePresenter }

{ #category : #'*Spec-Deprecated' }
ComposablePresenter >> instantiateModels: aCollectionOfPairs [
	
	"instantiateModels: is legacy code in ComposablePresenter and must not be used. It will be deprecated and removed."

	^ self instantiatePresenters: aCollectionOfPairs
]
