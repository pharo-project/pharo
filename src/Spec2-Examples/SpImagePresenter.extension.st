Extension { #name : #SpImagePresenter }

{ #category : #'*Spec2-Examples' }
SpImagePresenter class >> example [
	<sampleInstance>

	^ self new 
		image: (self iconNamed: #pharoBig);
		openWithSpec
]
