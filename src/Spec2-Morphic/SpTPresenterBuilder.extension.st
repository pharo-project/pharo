Extension { #name : #SpTPresenterBuilder }

{ #category : #'*Spec2-Morphic' }
SpTPresenterBuilder >> newImage [
	^ self instantiate: SpImagePresenter
]

{ #category : #'*Spec2-Morphic' }
SpTPresenterBuilder >> newLink [
	^ self instantiate: SpLinkPresenter
]

{ #category : #'*Spec2-Morphic' }
SpTPresenterBuilder >> newMorph [

	^ self instantiate: SpMorphPresenter
]
