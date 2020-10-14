Extension { #name : #SpLabelPresenter }

{ #category : #'*Spec2-Examples' }
SpLabelPresenter class >> example [
	"This example opens a spec window with a label."

	^ self new
		label: 'Label example';
		openWithSpec.
]
