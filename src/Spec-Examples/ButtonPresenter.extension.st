Extension { #name : #ButtonPresenter }

{ #category : #'*Spec-Examples' }
ButtonPresenter class >> example [
	<sampleInstance>
	^ self new
		openWithSpec;
		label: 'Click me'
]
