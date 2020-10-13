Extension { #name : #SpMorphPresenter }

{ #category : #'*Spec2-Morphic-Examples' }
SpMorphPresenter class >> example [

	^ self new 
		morph: (Morph new
			color: Color random;
			yourself);
		openWithSpec
]
