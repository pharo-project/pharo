Extension { #name : #LabelledDropList }

{ #category : #'*Spec-Examples' }
LabelledDropList class >> example [
	<sampleInstance>
	^ super example
]

{ #category : #'*Spec-Examples' }
LabelledDropList class >> setUpExample: example [
	example
		items: {'item 1' . 'item 2'};
		displayBlock: [ :item | item asString ];
		extent: 400 @ 50
]
