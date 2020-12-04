Extension { #name : #SpSliderInput }

{ #category : #'*Spec2-Examples' }
SpSliderInput class >> example [
	<sampleInstance>
	| example |
	example := self new
		min: 0;
		max: 250;
		value: 120;
		extent: 400 @ 50;
		openWithSpec.
	example withWindowDo: [ :window | window title: 'SliderInput example' ].
	^ example
]
