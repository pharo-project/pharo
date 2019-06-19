Extension { #name : #LabelledSliderInput }

{ #category : #'*Spec-Examples' }
LabelledSliderInput class >> example [
	<sampleInstance>
	^ super example
]

{ #category : #'*Spec-Examples' }
LabelledSliderInput class >> setUpExample: example [
	example
		min: 0;
		max: 250;
		autoAccept: true;
		value: 120;
		extent: 400 @ 50
]
