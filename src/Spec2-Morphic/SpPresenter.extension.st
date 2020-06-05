Extension { #name : #SpPresenter }

{ #category : #'*Spec2-Morphic' }
SpPresenter >> newImage [
	^ self instantiate: SpImagePresenter
]

{ #category : #'*Spec2-Morphic' }
SpPresenter >> newLink [
	^ self instantiate: SpLinkPresenter
]

{ #category : #'*Spec2-Morphic' }
SpPresenter >> newMorph [

	^ self instantiate: SpMorphPresenter
]
