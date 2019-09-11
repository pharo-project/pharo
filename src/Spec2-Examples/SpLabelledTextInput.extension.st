Extension { #name : #SpLabelledTextInput }

{ #category : #'*Spec2-Examples' }
SpLabelledTextInput class >> example [
	<sampleInstance>
	^ super example
]

{ #category : #'*Spec2-Examples' }
SpLabelledTextInput class >> setUpExample: example [
	example input placeholder: 'Tilt'.
	example extent: 400 @ 50
]
