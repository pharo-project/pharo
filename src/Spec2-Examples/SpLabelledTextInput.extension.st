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

{ #category : #'*Spec2-Examples' }
SpLabelledTextInput class >> simpleExample [
	<example>
	^ (self new
		label: 'simple text field'; 
		text: 'text to be edited';
		whenTextChanged:  
			[ :newText| Transcript nextPutAll: newText;cr;endEntry  ];
		whenSubmitDo: 
			[ :finalText |  Transcript nextPutAll: 'Done: ', finalText;cr;endEntry] )
	openWithSpec
]
