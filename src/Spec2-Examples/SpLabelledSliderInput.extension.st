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
		value: 120;
		extent: 400 @ 50
]

{ #category : #'*Spec2-Examples' }
SpLabelledSliderInput class >> simpleExample [
	<example>
	^ (self new
		layout: self labelLeft ;
		label: 'slider label';
		min: 10;
		max: 90;
		value: 25;
		whenValueChangedDo:  
			[ :new :old| Transcript nextPutAll: new asString;cr;endEntry  ] )
	openWithSpec
]
