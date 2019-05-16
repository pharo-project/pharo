Extension { #name : #LabelledTextInput }

{ #category : #'*Spec-Examples' }
LabelledTextInput class >> example [
	<sampleInstance>
	| example |
	example := self abstractExample.
	example input placeholder: 'Tilt'.
	example
		extent: 400 @ 50;
		openWithSpec.
	^ example
]
