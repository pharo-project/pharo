Extension { #name : #LabelledTextInput }

{ #category : #'*Spec-Examples' }
LabelledTextInput class >> example [
	<sampleInstance>
	^ super example
]

{ #category : #'*Spec-Examples' }
LabelledTextInput class >> setUpExample: example [
	example input placeholder: 'Tilt'.
	example extent: 400 @ 50
]
