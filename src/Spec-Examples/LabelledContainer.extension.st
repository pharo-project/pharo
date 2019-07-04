Extension { #name : #LabelledContainer }

{ #category : #'*Spec-Examples' }
LabelledContainer class >> abstractExample [
	| example |
	example := self new.
	example label: 'I am a label'.
	^ example
]

{ #category : #'*Spec-Examples' }
LabelledContainer class >> example [
	<sampleInstance>
	| example |
	example := self abstractExample.
	self setUpExample: example.
	example openDialogWithSpec.
	example withWindowDo: [ :window | window title: self name asString , ' example' ].
	^ example
]

{ #category : #'*Spec-Examples' }
LabelledContainer class >> setUpExample: example [
	example content: ButtonPresenter.
	example subwidget label: 'I am a button'
]
