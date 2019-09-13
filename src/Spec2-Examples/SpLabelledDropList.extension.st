Extension { #name : #SpLabelledDropList }

{ #category : #'*Spec2-Examples' }
SpLabelledDropList class >> example [
	<sampleInstance>
	^ super example
]

{ #category : #'*Spec2-Examples' }
SpLabelledDropList class >> setUpExample: example [
	example
		items: {'item 1' . 'item 2'};
		display: [ :item | item asString ];
		extent: 400 @ 50
]
