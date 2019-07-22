Extension { #name : #SpLabelledList }

{ #category : #'*Spec2-Examples' }
SpLabelledList class >> example [
	<sampleInstance>
	^ super example
]

{ #category : #'*Spec2-Examples' }
SpLabelledList class >> setUpExample: example [
	example items: {'item 1' . 'item 2'}
]
