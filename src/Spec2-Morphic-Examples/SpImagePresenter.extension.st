Extension { #name : #SpImagePresenter }

{ #category : #'*Spec2-Morphic-Examples' }
SpImagePresenter class >> example [
	<sampleInstance>

	^ self new 
		image: (self iconNamed: #pharoBig);
		openWithSpec
]
