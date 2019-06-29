Extension { #name : #SpLabelledSliderInput }

{ #category : #'*Spec2-Examples' }
SpLabelledSliderInput class >> example [
	<sampleInstance>
	^ super example
]

{ #category : #'*Spec2-Examples' }
SpLabelledSliderInput class >> setUpExample: example [
	example
		min: 0;
		max: 250;
		autoAccept: true;
		value: 120;
		extent: 400 @ 50
]
