Extension { #name : #LabelledContainer }

{ #category : #'*Spec-Examples' }
LabelledContainer class >> abstractExample [
	| example |
	example := self new.
	example
		title: self name asString , ' example';
		label: 'I am a label'.
	^ example
]

{ #category : #'*Spec-Examples' }
LabelledContainer class >> example [
	<sampleInstance>
	| example |
	example := self abstractExample.
	example content: ButtonPresenter.
	example subwidget
		label: 'I am a button'.
	example openDialogWithSpec.
	^ example
]
