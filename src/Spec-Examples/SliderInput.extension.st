Extension { #name : #SliderInput }

{ #category : #'*Spec-Examples' }
SliderInput class >> example [
	<sampleInstance>
	| example |
	example := self new
		min: 0;
		max: 250;
		autoAccept: true;
		value: 120;
		extent: 400 @ 50;
		openWithSpec.
	example withWindowDo: [ :window | window title: 'SliderInput example' ].
	^ example
]
