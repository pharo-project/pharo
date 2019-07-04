Extension { #name : #DatePresenter }

{ #category : #'*Spec-Examples' }
DatePresenter class >> example [
	<sampleInstance>
	^ self new openDialogWithSpec
]

{ #category : #'*Spec-Examples' }
DatePresenter class >> exampleModal [
	<sampleInstance>
	^ self new openDialogWithSpec modalRelativeTo: self currentWorld
]
