Extension { #name : #LabelledList }

{ #category : #'*Spec-Examples' }
LabelledList class >> example [
	<sampleInstance>
	^ super example
]

{ #category : #'*Spec-Examples' }
LabelledList class >> setUpExample: example [
	example items: {'item 1' . 'item 2'}
]
