Extension { #name : #TextPresenter }

{ #category : #'*Spec-Examples' }
TextPresenter class >> example [
	<sampleInstance>
	^ self new
	 openWithSpec;
		text:
			'This is a test.
If this had been an actual emergency...'

]
