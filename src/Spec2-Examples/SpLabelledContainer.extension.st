Extension { #name : #SpLabelledContainer }

{ #category : #'*Spec2-Examples' }
SpLabelledContainer class >> abstractExample [
	| example |
	example := self new.
	example label: 'I am a label'.
	^ example
]

{ #category : #'*Spec2-Examples' }
SpLabelledContainer class >> example [
	<sampleInstance>
	| example |
	example := self abstractExample.
	self setUpExample: example.
	example openDialogWithSpec.
	example withWindowDo: [ :window | window title: self name asString , ' example' ].
	^ example
]

{ #category : #'*Spec2-Examples' }
SpLabelledContainer class >> setUpExample: example [
	example content: SpButtonPresenter.
	example subwidget label: 'I am a button'
]
